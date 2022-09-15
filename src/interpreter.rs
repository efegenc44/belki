use std::collections::HashMap;
use std::vec;
use std::fs;
use std::process::exit;

use crate::core_module;
use crate::math_module;

use crate::value::Value;
use crate::ast::Node;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::error::runtime;

#[derive(Debug, Clone)]
struct ClassDef {
    name: String,
    members: Vec<String>,
    methods: HashMap<String, Function>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    env: HashMap<String, Value>,
    upper: Option<Box<Scope>>
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            env: HashMap::new(),
            upper: None
        }
    }

    fn assign(&mut self, var: &String, val: &Value) {
        match self.env.get_mut(var) {
            Some(value) => {
                *value = val.clone(); 
            },
            None => {
                match &mut self.upper {
                    Some(scope) => {
                        scope.assign(var, val);
                    },
                    None => {
                        runtime(format!(
                            "No variable named '{:?}'.", 
                            var).as_str()
                        );
                    } 
                }
            }
        }
    }

    fn resolve_value(&self, var: &String) -> Option<Value> {
        match self.env.get(var) {
            Some(value) => {
                Some(value.clone()) 
            },
            None => {
                match &self.upper {
                    Some(upper) => {
                        upper.resolve_value(var)
                    },
                    None => None
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Function {
    name: String,
    args: Vec<String>,
    body: Node,
    closure: Option<HashMap<String, Value>>
}

#[derive(Clone)]
pub struct NativeFunction {
    name: String,
    arity: usize,
    body: fn(&mut Interpreter, &[Value]) -> Value,
}

impl NativeFunction {
    pub fn new(name: String, arity: usize, body: fn(&mut Interpreter, &[Value]) -> Value) -> NativeFunction {
        NativeFunction { name, arity, body }
    }
}

pub struct Interpreter {
    current_scope: Scope,
    return_val   : Value,
    globals      : HashMap<String, Value>,
    
    instances    : HashMap<u64, HashMap<String, Value>>,
    lists        : HashMap<u64, Vec<Value>>,
    classes      : HashMap<u64, ClassDef>,
    modules      : HashMap<u64, Scope>,
    functions    : HashMap<u64, Function>,
    nfunctions   : HashMap<u64, NativeFunction>,

    ins_id       : u64,
    list_id      : u64,
    mod_id       : u64,
    class_id     : u64,
    func_id      : u64,
    nfunc_id     : u64,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { 
            current_scope: Scope::new(),
            globals      : HashMap::new(),
            return_val   : Value::None,
            
            instances : HashMap::new(),
            lists     : HashMap::new(),
            classes   : HashMap::new(),
            modules   : HashMap::new(),
            functions : HashMap::new(),
            nfunctions: HashMap::new(),

            ins_id   : 0,
            list_id  : 0,
            mod_id   : 0,
            class_id : 0,
            func_id  : 0,
            nfunc_id : 0,
        }
    }
    
    pub fn init(&mut self) {
        core_module::init(self);
        math_module::init(self);
    }

    fn enter_scope(&mut self) {
        let upper = self.current_scope.clone();
        self.current_scope = Scope::new();
        self.current_scope.upper = Some(Box::new(upper));
    }

    fn exit_scope(&mut self) {
        match &self.current_scope.upper {
            Some(scope) => {
                self.current_scope = *scope.clone()
            },
            None => unreachable!()
        }
    }

    pub fn get_list(&self, id: &u64) -> &Vec<Value> {
        self.lists.get(id).expect("No list with this id.")
    }

    pub fn add_native_function_to_module(&mut self, module_name: String, func: NativeFunction) {
        let module_val = self.eval(Node::Identifier(module_name)).unwrap();
        match module_val {
            Value::Module(id) => {
                let module = self.modules.get_mut(&id).unwrap();
                
                let id = self.nfunc_id;
                self.nfunc_id += 1;
                self.nfunctions.insert(id, func.clone());
                
                module.env.insert(func.name.clone(), Value::NativeFunction(id));
            } 
            _ => {}
        }
    }

    pub fn add_native_function(&mut self, func: NativeFunction) {
        let id = self.nfunc_id;
        self.nfunc_id += 1;
        self.nfunctions.insert(id, func.clone());

        self.globals.insert(func.name, Value::NativeFunction(id));
    }

    pub fn add_module(&mut self, global: bool, scope: Scope, name: String) {
        let id = self.mod_id;
        self.modules.insert(id, scope.clone());
        self.mod_id += 1;
        
        self.current_scope = Scope::new();
        if global {
            self.globals.insert(name, Value::Module(id));
        } 
    }
    
    
    pub fn eval(&mut self, node: Node) -> Option<Value> {
        match node {
            Node::Program(statements) => {
                for statement in statements {
                    self.eval(statement);
                }
                None
            },
            Node::Block(statements) => {
                self.enter_scope();
                for statement in statements {
                    self.eval(statement);
                }
                self.exit_scope();
                None
            },
            Node::FunBlock(statements) => {
                // self.enter_scope();
                for statement in statements {
                    match self.return_val {
                        Value::None => {
                            self.eval(statement);
                        },
                        _ => {
                            break;
                        }
                    }
                }
                // self.exit_scope();
                match self.return_val {
                    Value::Nothing => {},
                    _ => {
                        return Some(self.return_val.clone());
                    }
                }
                None
            },
            Node::Import(path) => {
                let source = fs::read_to_string(path.clone())
                    .expect("File read error");     
    
                let mut lexer = Lexer::new(source);
                let tokens = lexer.tokens();

                let mut parser = Parser::new(tokens);
                let node = parser.parse();
                //node.print(0);

                self.eval(node);
                
                let id = self.mod_id;
                self.modules.insert(id, self.current_scope.clone());
                self.mod_id += 1;
                
                self.current_scope = Scope::new();
                
                self.current_scope.env.insert(
                    path.split(".").collect::<Vec<_>>()[0].to_string(), 
                    Value::Module(id)
                );

                None
            },
            Node::Let { name, expr } => {
                let val = self.eval(*expr).unwrap();
                self.current_scope.env.insert(name, val);
                None
            },
            Node::Return(expr) => {
                self.return_val = self.eval(*expr).unwrap();
                None
                
            },

            Node::Class { name, members, methods } => {
                let mut values: HashMap<String, Function> = HashMap::new();
                
                for method in methods {
                    match method {
                        Node::Fun { name, args, body } => {
                            values.insert(
                                name.clone(),
                                Function { name, args, body: *body, closure: None }
                            );
                        }
                        _ => { }
                    }
                }
                
                let id = self.class_id;
                self.classes.insert(id, ClassDef { name: name.clone(), members, methods: values });
                self.class_id += 1;

                self.current_scope.env.insert(
                    name,
                    Value::ClassD(id)
                );
                None                
            },
            Node::Fun { name, args, body } => {
                let mut closure = HashMap::new();

                for (name, val) in self.current_scope.env.clone() {
                    closure.insert(name, val);
                }
                
                let id = self.func_id;
                self.functions.insert(id, Function { name: name.clone(), args, body: *body, closure: Some(closure) });
                self.func_id += 1;
                
                self.current_scope.env.insert(
                    name, 
                    Value::Function(id)
                );
                None
            },
            Node::If { expr, body, els } => {
                let val = self.eval(*expr).unwrap();
                match val {
                    Value::Bool(t) => {
                        if t {
                            self.eval(*body);
                        } else {
                            self.eval(*els);
                        }
                    },
                    _ => runtime("Expected bool expr in If")
                }
                None
            },
            Node::Else { expr, body, els } => {
                match *expr {
                    Node::None => { 
                        self.eval(*body); 
                        assert!(*els == Node::None, "Else Error");
                    },
                    _ => {
                        let val = self.eval(*expr).unwrap();
                        match val {
                            Value::Bool(t) => {
                                if t {
                                    self.eval(*body);
                                } else {
                                    self.eval(*els);
                                }
                            },
                            _ => runtime("Expected bool expr in Else")
                        }
                    }
                }
                None
            },
            Node::While { expr, body } => {
                loop {
                    let val = self.eval(*expr.clone()).unwrap();
                    match val {
                        Value::Bool(t) => {
                            if t {
                                self.eval(*body.clone());
                            } else {
                                break
                            }
                        },
                        _ => runtime("Expected bool expr in While")
                    }
                }
                None
            },
            Node::Binary { op, lhs, rhs } => {
                match op.as_str() {
                    "+" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::Int(a), Value::Int(b)) => {
                                Some(Value::Int(a + b))
                            }
                            _ => {None}
                        }
                    }, 
                    "-" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::Int(a), Value::Int(b)) => {
                                Some(Value::Int(a - b))
                            }
                            _ => {None}
                        }
                    },
                    "*" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::Int(a), Value::Int(b)) => {
                                Some(Value::Int(a * b))
                            }
                            _ => {None}
                        }
                    },
                    "/" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::Int(a), Value::Int(b)) => {
                                Some(Value::Int(a / b))
                            }
                            _ => {None}
                        }
                    },
                    "%" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::Int(a), Value::Int(b)) => {
                                Some(Value::Int(a % b))
                            }
                            _ => {None}
                        }
                    },
                    "<=" => match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                        (Value::Int(a), Value::Int(b)) => {
                            Some(Value::Bool(a <= b))
                        }
                        _ => {None},
                    },
                    ">=" => match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                        (Value::Int(a), Value::Int(b)) => {
                            Some(Value::Bool(a >= b))
                        }
                        _ => {None},
                    },
                    "<" => match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                        (Value::Int(a), Value::Int(b)) => {
                            Some(Value::Bool(a < b))
                        }
                        _ => {None},
                    },
                    ">" => match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                        (Value::Int(a), Value::Int(b)) => {
                            Some(Value::Bool(a > b))
                        }
                        _ => {None},
                    }
                    "!=" => match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                        (Value::Int(a), Value::Int(b)) => {
                            Some(Value::Bool(a != b))
                        }
                        _ => {None},
                    },
                    "==" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::Int(a), Value::Int(b)) => {
                                Some(Value::Bool(a == b))
                            }
                            _ => {None}
                        }
                    }, 
                    "||" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::Bool(a), Value::Bool(b)) => {
                                Some(Value::Bool(a || b))
                            }
                            _ => {None}
                        }
                    },
                    "&&" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::Bool(a), Value::Bool(b)) => {
                                Some(Value::Bool(a && b))
                            }
                            _ => {None}
                        }
                    },
                    "=" => {
                        let val_to_assign = self.eval(*rhs).unwrap();
                        match *lhs {
                            Node::Identifier(var) => {
                                self.current_scope.assign(&var, &val_to_assign);
                                Some(val_to_assign)
                            },
                            Node::Binary { op, lhs, rhs } => {
                                match op.as_str() {
                                    "." => {
                                        match (self.eval(*lhs), *rhs.clone()) {
                                            (Some(val), rhs) => {
                                                match (val, rhs) {
                                                    (Value::Instance(_class_name, id), Node::Identifier(member)) => {
                                                        let ins = self.instances.get_mut(&id).unwrap();
                                                        let attr = ins.get_mut(&member).unwrap();
                                                        *attr = val_to_assign.clone();
                                                        Some(val_to_assign)
                                                    }
                                                    (Value::Module(id), Node::Identifier(member)) => {
                                                        let module = self.modules.get_mut(&id).unwrap();
                                                        module.assign(&member, &val_to_assign);
                                                        Some(val_to_assign)
                                                    },
                                                    _ => None
                                                }
                                            },
                                            _ => None
                                        }
                                    },
                                    "[" => {
                                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                                            (Value::List(index), Value::Int(i)) => {
                                                let list = self.lists.get_mut(&index).unwrap();
                                                let _ = std::mem::replace(&mut list[i as usize], val_to_assign.clone());
                                                Some(val_to_assign)
                                            }
                                            _ => None
                                        }
                                    }
                                    _ => None
                                }
                            }
                            _ => None
                        }
                    },
                    "." => {
                        match (self.eval(*lhs.clone()), *rhs.clone()) {
                            (Some(val), rhs) => {
                                match (val, rhs) {
                                    (Value::Instance(class_id, id), Node::Identifier(member)) => {
                                        let ins = &self.instances[&id];
                                        match ins.get(&member) {
                                            Some(a) => Some(a.clone()),
                                            None => {
                                                let class = self.classes.get(&class_id).unwrap();
                                                if class.methods.contains_key(&member)  {
                                                    Some(Value::Method(id, class_id, member))
                                                } else {
                                                    None
                                                }
                                            }
                                        }
                                    }
                                    (Value::Module(id), Node::Identifier(member)) => {
                                        let scope = self.modules.get(&id).unwrap();
                                        scope.resolve_value(&member)
                                    }
                                    _ => None
                                }
                            },
                            _ => None
                        }
                    },
                    "[" => {
                        match (self.eval(*lhs).unwrap(), self.eval(*rhs).unwrap()) {
                            (Value::List(id), Value::Int(index)) => {
                                Some(self.lists.get(&id).unwrap()[index as usize].clone())
                            },
                            (Value::String(string), Value::Int(index)) => {
                                Some(Value::String(string.chars().nth(index as usize).unwrap().to_string()))
                            }
                            _ => None
                        }
                    },
                    "|>" => {
                            self.eval(Node::FunCall { fun: rhs, args: vec![*lhs] })
                        }
                    
                    _ => {None}, 
                }
            },
            Node::Integer(i) => Some(Value::Int(i)),
            Node::Float(f) => Some(Value::Float(f)),
            Node::List(nodes) => {
                let mut values: Vec<Value> = vec![];
                for node in nodes {
                    values.push(self.eval(node).unwrap());
                }
                
                let id = self.list_id;
                self.lists.insert(id, values);
                self.list_id += 1;
                Some(Value::List(id))
            },
            Node::String(s) => Some(Value::String(s)),
            Node::Identifier(s) => { 
                match self.current_scope.resolve_value(&s){
                    Some(value) => Some(value),
                    None => {
                        Some(self.globals.get(&s).unwrap().clone())
                    }
                }
            },
            Node::Group(expr) => {
                self.eval(*expr)
            },
            Node::Unary { op, operand } => {
                match op.as_str() {
                    "+" => {
                        match self.eval(*operand).unwrap() {
                            Value::Int(a) => {
                                Some(Value::Int(a))
                            }
                            _ => {None}
                        }
                    }, 
                    "-" => {
                        match self.eval(*operand).unwrap() {
                            Value::Int(a) => {
                                Some(Value::Int(-a))
                            }
                            _ => {None}
                        }
                    }, 
                    "!" => {
                        match self.eval(*operand).unwrap() {
                            Value::Bool(a) => {
                                Some(Value::Bool(!a))
                            }
                            _ => {None}
                        }
                    }, 
                    _ => {None}, 
                }
            },
            Node::FunCall { fun, args } => {
                let fun_val = self.eval(*fun.clone());
                let node_args = args;
                match fun_val {
                    Some(f) => {
                        match f {
                            Value::NativeFunction(id) => {
                                let mut values: Vec<Value> = vec![];
                                for arg in node_args.clone() {
                                    values.push(self.eval(arg).unwrap());
                                } 
                                let nfunction = self.nfunctions.get(&id).unwrap().clone();
                                
                                if nfunction.arity != node_args.len() {
                                    runtime(format!("Wrong number of argumnets for {}", nfunction.name).as_str());
                                }
                                let ret = (nfunction.body)(self, &values);
                                Some(ret)
                            }
                            Value::Function(id) => {
                                let function = self.functions.get(&id).unwrap().clone(); 
                                
                                self.enter_scope();
                                // recursiveness 
                                self.current_scope.env.insert(
                                    function.name.clone(), 
                                    Value::Function(id)
                                );
                                
                                if function.args.len() != node_args.len() {
                                    runtime(format!("Wrong number of argumnets for {}", function.name).as_str());

                                }
                                
                                for (name, val) in function.closure.unwrap() {
                                    self.current_scope.env.insert(name, val);
                                }

                                for i in 0..function.args.len() {
                                    let arg_val = self.eval(node_args[i].clone()).unwrap();
                                    self.current_scope.env.insert(
                                        function.args[i].clone(), arg_val.clone()
                                    );
                                }
                                
                                let ret = self.eval(function.body);

                                self.exit_scope();
                                self.return_val = Value::None;
                                ret
                            }
                            Value::ClassD(class_id) => {
                                let class = self.classes.get(&class_id).unwrap().clone();
                                let mut values: HashMap<String, Value> = HashMap::new();
                                
                                if class.members.len() != node_args.len() {
                                    runtime(format!("Wrong number of argumnets for {}", class.name).as_str());
                                }

                                for i in 0..class.members.len() {
                                    let arg_val = self.eval(node_args[i].clone()).unwrap();
                                    values.insert(
                                        class.members[i].clone(), arg_val.clone()
                                    );
                                }
                                let id = self.ins_id;
                                self.instances.insert(id, values);
                                self.ins_id += 1;
                                Some(Value::Instance(class_id, id))
                            },
                            Value::Method(ins_id, class_id, method_name) => {
                                let ins_val = Value::Instance(class_id, ins_id);
                                let method = self.classes.get(&class_id).unwrap().methods.get(&method_name).unwrap().clone();                                        
                                self.enter_scope();
                                
                                if method.args.len() - 1 != node_args.len() {
                                    runtime(format!("Wrong number of argumnets for method {}", method.name).as_str());
                                }
                                
                                if method.args[0] != "this".to_string() {
                                    println!("this error");
                                    exit(1);
                                }

                                self.current_scope.env.insert("this".to_string(), ins_val);

                                for i in 1..method.args.len() {
                                    let arg_val = self.eval(node_args[i-1].clone()).unwrap();
                                    self.current_scope.env.insert(
                                        method.args[i].clone(), arg_val.clone()
                                    );
                                }
                                
                                let ret = self.eval(method.body);
                                
                                self.exit_scope();
                                self.return_val = Value::None;
                                ret
                            }
                            _ => None
                        }
                    },
                    None => None 
                }
            },
            Node::True => Some(Value::Bool(true)),
            Node::False => Some(Value::Bool(false)),
            Node::Nothing => Some(Value::Nothing),
            Node::Print(expr) => {
                let val = self.eval(*expr).unwrap();
                match val {
                    Value::Instance(_c, id) => {
                        let ins = self.instances.get(&id).unwrap();
                        println!("{:?}", ins);
                    },
                    Value::List(id) => {
                        let list = self.lists.get(&id).unwrap();
                        println!("{:?}", list);
                    }
                    _ => {
                        println!("{:?}", val);
                    }
                }
                None
            }
            Node::None => None,
        }
    }
    
}