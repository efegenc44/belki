use std::collections::HashMap;
use std::process::exit;
use std::vec;
use std::fs;

use crate::core_module;
use crate::math_module;

use crate::value::Value;
use crate::ast::Node;
use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(Debug)]
pub enum RuntimeError {
    TypeMismatch,
    ArgNumMismatch,
    AssignmentError,
    MemberAccessError,
    ElementAccessError,
    UndefinedVariable,
    AbsendThisError,
    AlreadyDefinedVarible,
    DivisionByZero,
    AssertionFailure,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    env: HashMap<String, Value>,
    upper: Option<Box<Scope>>
}

impl Scope {
    pub fn new() -> Scope {
        Scope { env: HashMap::new(), upper: None }
    }

    fn assign(&mut self, var: &String, val: &Value) -> Result<(), RuntimeError> {
        match self.env.get_mut(var) {
            Some(value) => {
                *value = val.clone(); Ok(())
            }
            None => match &mut self.upper {
                Some(scope) => {
                    scope.assign(var, val)
                },
                None => Err(RuntimeError::UndefinedVariable)
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
#[allow(dead_code)]
struct ClassDef {
    name: String,
    members: Vec<String>,
    methods: HashMap<String, Function>
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    args: Vec<String>,
    body: Node,
    closure: Option<HashMap<String, Value>>
}

impl Function {
    pub fn call(&self, func_id: u64, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        interpreter.enter_scope();
        // recursiveness 
        interpreter.current_scope.env.insert(
            self.name.clone(), 
            Value::Function(func_id)
        );
        if self.args.len() != args.len() {
            return Err(RuntimeError::ArgNumMismatch);
        }
        for (name, val) in self.closure.clone().unwrap() {
            interpreter.current_scope.env.insert(name, val);
        }
        for i in 0..self.args.len() {
            interpreter.current_scope.env.insert(
                self.args[i].clone(), args[i].clone()
            );
        }
        let ret = interpreter.eval(self.body.clone());
        interpreter.exit_scope();
        interpreter.return_val = Value::None;
        ret
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    name: String,
    arity: usize,
    body: fn(&mut Interpreter, &[Value]) -> Result<Value, RuntimeError>,
}

impl NativeFunction {
    pub fn new(name: String, arity: usize, body: fn(&mut Interpreter, &[Value]) -> Result<Value, RuntimeError>) -> NativeFunction {
        NativeFunction { name, arity, body }
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        if self.arity != args.len() {
            return Err(RuntimeError::ArgNumMismatch);
        }
        let ret = (self.body)(interpreter, args);
        ret
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
        // math_module::init(self);
    }

    fn enter_scope(&mut self) {
        let upper = self.current_scope.clone();
        self.current_scope = Scope::new();
        self.current_scope.upper = Some(Box::new(upper));
    }

    fn exit_scope(&mut self) {
        match &self.current_scope.upper {
            Some(scope) => 
                self.current_scope = *scope.clone(),
            None => unreachable!()
        }
    }

    pub fn get_list(&self, id: &u64) -> &Vec<Value> {
        self.lists.get(id).expect("No list with this id.")
    }

    pub fn get_list_mut(&mut self, id: &u64) -> &mut Vec<Value> {
        self.lists.get_mut(id).expect("No list with this id.")
    }

    pub fn get_function(&self, id: &u64) -> &Function {
        self.functions.get(id).expect("No Function with this id.")
    }

    pub fn get_nfunction(&self, id: &u64) -> &NativeFunction {
        self.nfunctions.get(id).expect("No Native Function with this id.")
    }

    pub fn add_native_function_to_module(&mut self, module_name: String, func: NativeFunction) {
        let module_val = self.eval(Node::Identifier(module_name))
            .expect("Expected Value");
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
        // self.current_scope = Scope::new();
        if global {
            self.globals.insert(name, Value::Module(id));
        } 
    }
    
    fn add_function(&mut self, function: Function) {
        let id = self.func_id;
        self.functions.insert(id, function.clone());
        self.func_id += 1;
        
        self.current_scope.env.insert(
            function.name, 
            Value::Function(id)
        );
    }

    pub fn eval(&mut self, node: Node) -> Result<Value, RuntimeError> {
        match node {
            Node::Program(statements) => {
                for statement in statements {
                    self.eval(statement)?;
                } Ok(Value::None)
            },
            Node::Block(statements) => {
                self.enter_scope();
                for statement in statements {
                    match self.return_val {
                        Value::None => {self.eval(statement)?;},
                        _ => break,   
                    }
                }
                self.exit_scope(); 
                Ok(Value::None)
            },
            Node::FunBlock(statements) => {
                for statement in statements {
                    match self.return_val {
                        Value::None => {self.eval(statement)?;},
                        _ => break,   
                    }
                }
                Ok(self.return_val.clone())
            },
            Node::Import(path) => {
                if path == "math" {
                    math_module::init(self);
                    return Ok(Value::None);
                }
                
                let source = fs::read_to_string(path.clone())
                    .expect("File read error");     
    

                let mut lexer = Lexer::new(source);
                let tokens = match lexer.tokens() {
                    Ok(tokens) => tokens,
                    Err(error) => {
                        println!("{:?}", error);
                        exit(0);
                    }
                };
                let mut parser = Parser::new(tokens);
                let node = match parser.parse() {
                    Ok(node) => node,
                    Err(error) => {
                        println!("{:?}", error);
                        exit(0)
                    }
                };
                self.eval(node)?;
                
                let id = self.mod_id;
                self.modules.insert(id, self.current_scope.clone());
                self.mod_id += 1;
                self.current_scope = Scope::new();
                self.current_scope.env.insert(
                    path
                    .split("/").collect::<Vec<_>>().last().unwrap().to_string()    
                    .split(".").collect::<Vec<_>>()[0].to_string(), 
                    Value::Module(id)
                );
                Ok(Value::None)
            },
            Node::Let { name, expr } => {
                let val = self.eval(*expr)?;
                // Maybe allow shadowing?
                if !self.current_scope.env.contains_key(&name) {
                    self.current_scope.env.insert(name, val); Ok(Value::None)
                } else { Err(RuntimeError::AlreadyDefinedVarible) }
            },
            Node::Return(expr) => {
                self.return_val = self.eval(*expr)?;
                Ok(Value::None)
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
                Ok(Value::None)                
            },
            Node::Fun { name, args, body } => {
                let closure = self.current_scope.env.clone();
                self.add_function(Function { name, args, body: *body, closure: Some(closure) });
                Ok(Value::None)
            },
            Node::If { expr, body, els } => {
                let val = self.eval(*expr)?;
                match val {
                    Value::Bool(t) => {
                        if t {
                            assert!(self.eval(*body).unwrap() == Value::None, "If didn't returned None");
                        } else {
                            assert!(self.eval(*els).unwrap() == Value::None, "If didn't returned None");
                        }
                        Ok(Value::None)
                    },
                    _ => Err(RuntimeError::TypeMismatch)
                }
            },
            Node::Else { expr, body, els } => {
                match *expr {
                    Node::None => { 
                        assert!(self.eval(*body).unwrap() == Value::None, "Else didn't returned None");
                        assert!(*els == Node::None, "Unexpected Else Statement.");
                        Ok(Value::None)
                    },
                    _ => {
                        let val = self.eval(*expr)?;
                        match val {
                            Value::Bool(t) => {
                                if t {
                                    assert!(self.eval(*body).unwrap() == Value::None, "Else didn't returned None");
                                } else {
                                    assert!(self.eval(*els).unwrap() == Value::None, "Else didn't returned None");
                                }
                                Ok(Value::None)
                            },
                            _ => Err(RuntimeError::TypeMismatch)
                        }
                    }
                }
            },
            Node::While { expr, body } => {
                while self.return_val == Value::None {
                    let val = self.eval(*expr.clone())?;
                    match val {
                        Value::Bool(t) => {
                            if t {
                                assert!(self.eval(*body.clone()).unwrap() == Value::None, "While didn't returned None");
                            } else { return Ok(Value::None) }
                        },
                        _ => return Err(RuntimeError::TypeMismatch)
                    }
                }
                Ok(Value::None)
            },
            Node::Binary { op, lhs, rhs } => {
                let lhs_value = self.eval(*lhs.clone())?;
                let mut rhs_value = Value::None;
                if op.as_str() != "." {
                    rhs_value = self.eval(*rhs.clone())?;
                }
                match op.as_str() {
                    "+" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    } 
                    "-" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "*" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "/" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => {
                            if b != 0 { Ok(Value::Int(a / b)) } 
                            else { Err(RuntimeError::DivisionByZero) }
                        }
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "%" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "<=" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    ">=" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "<" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    ">" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "!=" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a != b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a != b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "==" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a == b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "||" => match (lhs_value, rhs_value) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "&&" => match (lhs_value, rhs_value) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "=" => match *lhs {
                        Node::Identifier(var) => {
                            self.current_scope.assign(&var, &rhs_value)?;
                            Ok(rhs_value)
                        },
                        Node::Binary { op, lhs, rhs } => {
                            let llhs_value = self.eval(*lhs.clone())?;
                            let mut lrhs_value = Value::None;
                            if op.as_str() != "." {
                                lrhs_value = self.eval(*rhs.clone())?;
                            }
                            match op.as_str() {
                                "." => match (llhs_value, *rhs) {
                                    (Value::Instance(_, id), Node::Identifier(member)) => {
                                        let ins = self.instances.get_mut(&id)
                                            .expect("Instance couldn't find.");
                                        let attr = ins.get_mut(&member).unwrap();
                                        *attr = rhs_value.clone();
                                        Ok(rhs_value)
                                    }
                                    (Value::Module(id), Node::Identifier(member)) => {
                                        let module = self.modules.get_mut(&id)
                                            .expect("Module couldn't find.");
                                        module.assign(&member, &rhs_value)?;
                                        Ok(rhs_value)
                                    },
                                    _ => Err(RuntimeError::MemberAccessError)
                                }
                                "[" => match (llhs_value, lrhs_value) {
                                    (Value::List(index), Value::Int(i)) => {
                                        let list = self.lists.get_mut(&index)
                                            .expect("List couldn't find.");
                                        let _ = std::mem::replace(&mut list[i as usize], rhs_value.clone());
                                        Ok(rhs_value)
                                    }
                                    _ => Err(RuntimeError::ElementAccessError)
                                }
                                _ => Err(RuntimeError::AssignmentError)
                            }
                        }
                        _ => Err(RuntimeError::AssignmentError)
                    },
                    "." => match (lhs_value, *rhs.clone()) {                            
                        (Value::Instance(class_id, id), Node::Identifier(member)) => {
                            let ins = &self.instances[&id];
                            match ins.get(&member) {
                                Some(val) => Ok(val.clone()),
                                None => {
                                    let class = self.classes.get(&class_id)
                                        .expect("Class definition couldn't find.");
                                    if class.methods.contains_key(&member) {
                                        Ok(Value::Method(id, class_id, member))
                                    } else { Err(RuntimeError::MemberAccessError) }
                                }
                            }
                        }
                        (Value::Module(id), Node::Identifier(member)) => {
                            let scope = self.modules.get(&id)
                                .expect("Module couldn't find.");
                            let res = scope.resolve_value(&member);
                            if res.is_none() {
                                Err(RuntimeError::MemberAccessError)
                            } else { Ok(res.unwrap()) }
                        }
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "[" => match (lhs_value, rhs_value) {
                        (Value::List(id), Value::Int(index)) => {
                            Ok(self.get_list(&id)[index as usize].clone())
                        },
                        (Value::String(string), Value::Int(index)) => {
                            let res = string.chars().nth(index as usize);
                            if res.is_none() { Err(RuntimeError::ElementAccessError) }
                            else { Ok(Value::String(res.unwrap().to_string())) }
                        }
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "|>" => match rhs_value {
                        Value::Function(id) => {
                            let function = self.get_function(&id).clone();
                            function.call(id, self, &[lhs_value])
                        },
                        Value::NativeFunction(id) => {
                            let nfunction = self.get_nfunction(&id).clone();
                            nfunction.call(self, &[lhs_value])
                        }
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    _ => unreachable!() 
                }
            },
            Node::Integer(i) => Ok(Value::Int(i)),
            Node::Float(f) => Ok(Value::Float(f)),
            Node::List(nodes) => {
                let mut values: Vec<Value> = vec![];
                for node in nodes {
                    values.push(self.eval(node).expect("Expected Value"));
                }
                
                let id = self.list_id;
                self.lists.insert(id, values);
                self.list_id += 1;
                Ok(Value::List(id))
            },
            Node::String(s) => Ok(Value::String(s)),
            Node::Identifier(s) => { 
                match self.current_scope.resolve_value(&s){
                    Some(value) => Ok(value),
                    None => {
                        match self.globals.get(&s) {
                            Some(val) => Ok(val.clone()),
                            None => Err(RuntimeError::UndefinedVariable)
                        }
                    }
                }
            },
            Node::Group(expr) => self.eval(*expr),
            Node::Unary { op, operand } => {
                let operand_val = self.eval(*operand)?;
                match op.as_str() {
                    "+" => match operand_val {
                        Value::Int(a) => Ok(Value::Int(a)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "-" => match operand_val {
                        Value::Int(a) => Ok(Value::Int(-a)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "!" => match operand_val {
                        Value::Bool(a) => Ok(Value::Bool(!a)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    _ => unreachable!()
                }
            },
            Node::FunCall { fun, args } => {
                let fun_val = self.eval(*fun.clone())?;
                let node_args = args;
                match fun_val {
                    Value::NativeFunction(id) => {
                        let nfunction = self.nfunctions.get(&id).unwrap().clone();
                        let values = node_args.iter().map(|arg| {
                            self.eval(arg.clone())
                                .expect("Value Expected")
                        }).collect::<Vec<_>>(); 
                        
                        nfunction.call(self, &values)
                    }
                    Value::Function(id) => {
                        let function = self.get_function(&id).clone(); 
                        let values = node_args.iter().map(|arg| {
                            self.eval(arg.clone())
                                .expect("Value Expected")
                        }).collect::<Vec<_>>();

                        function.call(id, self, &values)
                    }
                    Value::ClassD(class_id) => {
                        let class = self.classes.get(&class_id).unwrap().clone();
                        let mut values: HashMap<String, Value> = HashMap::new();
                        
                        if class.members.len() != node_args.len() {
                            return Err(RuntimeError::ArgNumMismatch);
                        }

                        for i in 0..class.members.len() {
                            let arg_val = self.eval(node_args[i].clone())
                                .expect("Value Expected");
                            values.insert(
                                class.members[i].clone(), arg_val.clone()
                            );
                        }
                        let id = self.ins_id;
                        self.instances.insert(id, values);
                        self.ins_id += 1;
                        Ok(Value::Instance(class_id, id))
                    },
                    Value::Method(ins_id, class_id, method_name) => {
                        let ins_val = Value::Instance(class_id, ins_id);
                        let method = self.classes.get(&class_id).unwrap().methods.get(&method_name).unwrap().clone();                                        
                        self.enter_scope();
                        
                        if method.args.len() - 1 != node_args.len() {
                            return Err(RuntimeError::ArgNumMismatch);
                        }
                        
                        if method.args[0] != "this".to_string() {
                            return Err(RuntimeError::AbsendThisError);
                        }

                        self.current_scope.env.insert("this".to_string(), ins_val);

                        for i in 1..method.args.len() {
                            let arg_val = self.eval(node_args[i-1].clone())
                            .expect("Value Expected");
                            self.current_scope.env.insert(
                                method.args[i].clone(), arg_val.clone()
                            );
                        }
                        
                        let ret = self.eval(method.body);
                        
                        self.exit_scope();
                        self.return_val = Value::None;
                        ret
                    }
                    _ => Err(RuntimeError::TypeMismatch)
                }
            },
            Node::True => Ok(Value::Bool(true)),
            Node::False => Ok(Value::Bool(false)),
            Node::Nothing => Ok(Value::Nothing),
            Node::None => Ok(Value::None)
        }
    }
    
}