use std::collections::HashMap;
use std::process::exit;
use std::vec;
use std::fs;

use crate::core_module;
use crate::math_module;

use crate::value::{ Value, Type, KeyValue };
use crate::ast::Node;
use crate::lexer::Lexer;
use crate::parser::Parser;

type Symbol      = String;
type List        = Vec<Value>;
type EvalResult  = Result<Value, State>;
type Environment = HashMap<String, Value>; 
type Map         = HashMap<KeyValue, Value>; 

#[derive(Debug)]
pub enum RuntimeError {
    TypeMismatch,
    ArgNumMismatch,
    AssignmentError,
    MemberAccessError,
    IndexOutOfRange,
    UndefinedVariable,
    AlreadyDefinedVarible,
    DivisionByZero,
    AssertionFailure,
    BadRange,
    KeyError,
    HashError,
    ReturnedError,
    IllegalContinue,
    IllegalBreak,
    IllegalReturn,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::TypeMismatch          => write!(f, "\n  An error occured at Runtime:\n      Type Mismatch\n"),
            RuntimeError::ArgNumMismatch        => write!(f, "\n  An error occured at Runtime:\n      Wrong Number of Arguments\n"),
            RuntimeError::AssignmentError       => write!(f, "\n  An error occured at Runtime:\n      Assignment Error\n"),
            RuntimeError::MemberAccessError     => write!(f, "\n  An error occured at Runtime:\n      Member Access Error\n"),
            RuntimeError::IndexOutOfRange       => write!(f, "\n  An error occured at Runtime:\n      Index Out of Range\n"),
            RuntimeError::UndefinedVariable     => write!(f, "\n  An error occured at Runtime:\n      Undefined Variable\n"),
            RuntimeError::AlreadyDefinedVarible => write!(f, "\n  An error occured at Runtime:\n      Already Defined Variable\n"),
            RuntimeError::DivisionByZero        => write!(f, "\n  An error occured at Runtime:\n      Division By Zero Error\n"),
            RuntimeError::AssertionFailure      => write!(f, "\n  An error occured at Runtime:\n      Assertion Failed\n"),
            RuntimeError::BadRange              => write!(f, "\n  An error occured at Runtime:\n      Improper Range\n"),
            RuntimeError::KeyError              => write!(f, "\n  An error occured at Runtime:\n      Improper Key Value \n"),
            RuntimeError::HashError             => write!(f, "\n  An error occured at Runtime:\n      Unhashable Value\n"),
            RuntimeError::ReturnedError         => write!(f, "\n  An error occured at Runtime:\n      Expression Returned Error\n"),
            RuntimeError::IllegalContinue       => write!(f, "\n  An error occured at Runtime:\n      Illegal Continue\n"),
            RuntimeError::IllegalBreak          => write!(f, "\n  An error occured at Runtime:\n      Illegal Break\n"),
            RuntimeError::IllegalReturn         => write!(f, "\n  An error occured at Runtime:\n      Illegal Return\n"),
        }
    }
}

#[derive(Debug)]
pub enum State {
    Return(Value),
    Continue,
    Break,
    Error(RuntimeError)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    env: Environment,
    upper: Option<Box<Scope>>
}

pub struct Module {
    pub name: Symbol,
    pub scope: Scope
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
                Some(scope) => scope.assign(var, val),
                None => Err(RuntimeError::UndefinedVariable)
            }
        }
    }

    fn resolve(&self, var: &String) -> Option<Value> {
        match self.env.get(var) {
            Some(value) => Some(value.clone()), 
            None => match &self.upper {
                Some(upper) => upper.resolve(var),
                None => None
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Record {
    pub name: Symbol,
    pub members: Vec<Symbol>,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    pub args: Vec<Symbol>,
    pub body: Node,
    pub closure: Option<Environment>
}
#[derive(Clone)]
pub struct NativeFunction {
    pub name: Symbol,
    pub arity: usize,
    pub body: fn(&mut Interpreter, &[Value]) -> EvalResult,
}

impl NativeFunction {
    pub fn new(name: String, arity: usize, body: fn(&mut Interpreter, &[Value]) -> EvalResult) -> NativeFunction {
        NativeFunction { name, arity, body }
    }
}

impl Value {
    pub fn apply(&self, interpreter: &mut Interpreter, args: &[Value]) -> EvalResult {
        match self {
            Value::Function(id) => {
                let function = interpreter.get_function(&id).clone();
                if function.args.len() != args.len() {
                    return Err(State::Error(RuntimeError::ArgNumMismatch));
                }
                interpreter.enter_scope();
                // -- HACK for prevent from shadowing => Capture only needed variables
                if let Some(scope) = &function.closure {
                    interpreter.current_scope.env.extend(scope.clone());    
                }
                // --
                // self-reference 
                if function.name != "lambda" {
                    interpreter.current_scope.env.insert(
                        function.name.clone(), 
                        Value::Function(*id)
                    );
                }
                for i in 0..function.args.len() {
                    interpreter.current_scope.env.insert(
                        function.args[i].clone(), args[i].clone()
                    );
                }
                let ret = match interpreter.eval(function.body) {
                    Ok(v) => Ok(v),
                    Err(s) => match s {
                        State::Return(v) => Ok(v),
                        _ => Err(s)
                    }
                };
                
                interpreter.exit_scope();
                ret
            },
            Value::NativeFunction(id) => {
                let nfunction = interpreter.get_nfunction(&id).clone();
                if nfunction.arity != args.len() {
                    return Err(State::Error(RuntimeError::ArgNumMismatch));
                }
                (nfunction.body)(interpreter, args)
            },
            Value::Type(typ) => {
                let class_id = match typ {
                    Type::Custom(id) => id,
                    _ => {unreachable!()}
                };
                let class = interpreter.get_record(&class_id);
                if class.members.len() != args.len() {
                    return Err(State::Error(RuntimeError::ArgNumMismatch));
                }
                let mut values = HashMap::new();
                for i in 0..class.members.len() {
                    values.insert(class.members[i].clone(), args[i].clone());
                }
                let id = interpreter.ins_id;
                interpreter.instances.insert(id, values);
                interpreter.ins_id += 1;
                Ok(Value::Instance(*class_id, id))
            }
            _ => Err(State::Error(RuntimeError::TypeMismatch))
        }
    }

    fn equality(self, rhs_value: Value, interpreter: &mut Interpreter) -> EvalResult {
        match (self.clone(), rhs_value.clone()) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a == b as f32)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 == b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
            (Value::List(ida), Value::List(idb)) => Ok(Value::Bool(interpreter.get_list(&ida) == interpreter.get_list(&idb))),
            (Value::Map(ida), Value::Map(idb)) => Ok(Value::Bool(interpreter.get_map(&ida) == interpreter.get_map(&idb))),
            (Value::Type(type1), Value::Type(type2)) => Ok(Value::Bool(type1 == type2)),
            (Value::Range(a, b), Value::Range(c, d)) => Ok(Value::Bool(a == c && b == d)),
            _ => {
                if std::mem::discriminant(&self) != 
                    std::mem::discriminant(&rhs_value) {
                        return Ok(Value::Bool(false));
                    }
                Err(State::Error(RuntimeError::TypeMismatch))
            }
        }
    }
}

pub struct Interpreter {
    current_scope: Scope,
    globals      : Environment,
    
    repl         : bool,

    pub error_node: Node,

    instances    : HashMap<usize, Environment>,
    lists        : HashMap<usize, List>,
    records      : HashMap<usize, Record>,
    modules      : HashMap<usize, Module>,
    functions    : HashMap<usize, Function>,
    nfunctions   : HashMap<usize, NativeFunction>,
    maps         : HashMap<usize, Map>,

    ins_id       : usize,
    list_id      : usize,
    mod_id       : usize,
    record_id    : usize,
    func_id      : usize,
    nfunc_id     : usize,
    map_id       : usize,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { 
            current_scope: Scope::new(),
            globals      : Environment::new(),
            
            error_node: Node::None,

            repl: false,

            instances : HashMap::new(),
            lists     : HashMap::new(),
            records   : HashMap::new(),
            modules   : HashMap::new(),
            functions : HashMap::new(),
            nfunctions: HashMap::new(),
            maps      : HashMap::new(),

            ins_id   : 0,
            list_id  : 0,
            mod_id   : 0,
            record_id: 0,
            func_id  : 0,
            nfunc_id : 0,
            map_id   : 0,
        }
    }
    
    pub fn repl_mode(&mut self) {
        self.repl = true;
    }

    pub fn init(&mut self) {
        core_module::init(self);
    }

    fn enter_scope(&mut self) {
        let upper = self.current_scope.clone();
        self.current_scope = Scope::new();
        self.current_scope.upper = Some(Box::new(upper));
    }

    fn exit_scope(&mut self) {
        if let Some(scope) = &self.current_scope.upper {
            self.current_scope = *scope.clone();
        }
    }

    // Immutable getters
    pub fn get_list(&self, id: &usize) -> &List {
        self.lists.get(id).expect("No list with this id.")
    }

    pub fn get_function(&self, id: &usize) -> &Function {
        self.functions.get(id).expect("No Function with this id.")
    }

    pub fn get_nfunction(&self, id: &usize) -> &NativeFunction {
        self.nfunctions.get(id).expect("No Native Function with this id.")
    }

    pub fn get_record(&self, id: &usize) -> &Record {
        self.records.get(id).expect("No Class with this id.")
    }

    pub fn get_module(&self, id: &usize) -> &Module {
        self.modules.get(id).expect("No module with this id.")
    }

    pub fn get_instance(&self, id: &usize) -> &Environment {
        self.instances.get(id).expect("No Instance with this id.")
    }

    pub fn get_map(&self, id: &usize) -> &Map {
        self.maps.get(id).expect("No Map with this id.")
    }
    
    // Mutable getters
    pub fn get_list_mut(&mut self, id: &usize) -> &mut List {
        self.lists.get_mut(id).expect("No list with this id.")
    }

    pub fn get_map_mut(&mut self, id: &usize) -> &mut Map {
        self.maps.get_mut(id).expect("No map with this id.")
    }

    pub fn get_module_mut(&mut self, id: &usize) -> &mut Module {
        self.modules.get_mut(id).expect("No list with this id.")
    }

    pub fn get_instance_mut(&mut self, id: &usize) -> &mut Environment {
        self.instances.get_mut(id).expect("No list with this id.")
    }

    pub fn add_native_function_to_module(&mut self, module_name: String, func: NativeFunction) {
        let module_val = self.eval(Node::Identifier(module_name.clone()))
            .expect(&format!("No Module Named {}", module_name));
        match module_val {
            Value::Module(id) => {
                let module = self.modules.get_mut(&id).unwrap();
                let id = self.nfunc_id;
                self.nfunc_id += 1;
                self.nfunctions.insert(id, func.clone());
                module.scope.env.insert(func.name.clone(), Value::NativeFunction(id));
            } 
            _ => unreachable!()
        }
    }

    pub fn add_variable_to_module(&mut self, module_name: String, name: String, value: Value) {
        let module_val = self.eval(Node::Identifier(module_name.clone()))
            .expect(&format!("No Module Named {}", module_name));
        match module_val {
            Value::Module(id) => {
                let module = self.modules.get_mut(&id).unwrap();
                module.scope.env.insert(name, value);
            } 
            _ => unreachable!()
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
        self.modules.insert(id, Module {name: name.clone(), scope: scope.clone()});
        self.mod_id += 1;
        if global {
            self.globals.insert(name, Value::Module(id));
        } else {
            self.current_scope.env.insert(
                name, 
                Value::Module(id)
            );
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

    pub fn add_record(&mut self, record: Record) {
        let id = self.record_id;
        self.records.insert(id, record.clone());
        self.record_id += 1;
        self.current_scope.env.insert(
            record.name,
            Value::Type(Type::Custom(id))
        );
    }

    pub fn add_list(&mut self, list: List) -> Value {
        let id = self.list_id;
        self.lists.insert(id, list.clone());
        self.list_id += 1;
        return Value::List(id);
    }

    pub fn add_map(&mut self, map: Map) -> Value {
        let id = self.map_id;
        self.maps.insert(id, map.clone());
        self.map_id += 1;
        return Value::Map(id);
    }

    pub fn add_global_variable(&mut self, name: String, value: Value) {
        self.globals.insert(name, value);
    }
    
    fn collect_values(&mut self, nodes: Vec<Node>) -> Result<Vec<Value>, State> {
        let mut values = vec![];
        for node in nodes { 
            values.push(self.eval(node)?); 
        }; Ok(values)
    }

    pub fn eval(&mut self, node: Node) -> EvalResult {
        match node.clone() {
            Node::Module(statements) => {
                self.error_node = node;
                for statement in statements {
                    match self.eval(statement) {
                        Ok(v) => if self.repl && v != Value::None {
                            println!("{}", v.get_string(self));
                        },
                        Err(err) => match err {
                            State::Return(_) => return Err(State::Error(RuntimeError::IllegalReturn)),
                            State::Continue => return Err(State::Error(RuntimeError::IllegalContinue)),
                            State::Break => return Err(State::Error(RuntimeError::IllegalBreak)),
                            State::Error(_) => return Err(err)
                        }
                    }
                } 
                Ok(Value::None)
            },
            Node::Block(statements) => {
                self.enter_scope();
                for statement in statements {
                    self.eval(statement)?;
                }
                self.exit_scope();
                Ok(Value::None)
            },
            Node::ModuleDeclaration { name, body } => {
                self.enter_scope();
                let up = self.current_scope.upper.clone();
                self.current_scope.upper = Some(Box::new(Scope::new()));
                self.eval(*body)?;
                let scope = self.current_scope.clone();
                self.current_scope.upper = up;
                self.exit_scope();
                
                self.add_module(false, scope, name);
                Ok(Value::None)
            }
            Node::Import(path) => {
                if path == "math" {
                    math_module::init(self);
                    return Ok(Value::None);
                }
                
                let source = fs::read_to_string(path.clone())
                    .expect("\n  Error while reading the file\n");     
    

                let mut lexer = Lexer::new(source);
                let tokens = match lexer.tokens() {
                    Ok(tokens) => tokens,
                    Err(error) => {
                        println!("{}", error);
                        exit(0);
                    }
                };
                let mut parser = Parser::new(tokens);
                let node = match parser.parse() {
                    Ok(node) => node,
                    Err(error) => {
                        println!("{}", error);
                        exit(0)
                    }
                };
                let cs = self.current_scope.clone();
                self.current_scope = Scope::new();
                self.eval(node)?;
                let scope = self.current_scope.clone();
                self.current_scope = cs;
                let name = path
                    .split("/").collect::<Vec<_>>().last().unwrap().to_string()    
                    .split(".").collect::<Vec<_>>()[0].to_string();
                
                self.add_module(false, scope, name);
                Ok(Value::None)
            },
            Node::MapLiteral(map) => {
                self.error_node = node;
                let mut hmap = HashMap::new();
                for (k, v) in map {
                    let key = self.eval(k)?.to_keyvalue(); 
                    match key {
                        Some(_) => {hmap.insert(key.unwrap(), self.eval(v)?);},
                        None => return Err(State::Error(RuntimeError::HashError)),
                    }
                }
                Ok(self.add_map(hmap))
            }
            Node::ForStatement { var, iter, body } => {
                self.error_node = node;
                match self.eval(*iter)? {
                    Value::List(id) => {
                        self.enter_scope();
                        let list = self.get_list(&id).clone();
                        for i in list {
                            self.current_scope.env.insert(var.clone(), i.clone());
                            match self.eval(*body.clone()) {
                                Ok(_) => {},
                                Err(s) => match s {
                                    State::Continue => continue,
                                    State::Break => break,
                                    _ => return Err(s)
                                }
                            };
                        }
                        self.exit_scope();
                    },
                    Value::String(string) => {
                        self.enter_scope();
                        for (_, i) in string.chars().enumerate() {
                            self.current_scope.env.insert(var.clone(), Value::String(i.into()));
                            match self.eval(*body.clone()) {
                                Ok(_) => {},
                                Err(s) => match s {
                                    State::Continue => continue,
                                    State::Break => break,
                                    _ => return Err(s)
                                }
                            };
                        }
                        self.exit_scope();
                    },
                    Value::Range(a, b) => {
                        self.enter_scope();
                        for i in a..b {
                            self.current_scope.env.insert(var.clone(), Value::Int(i));
                            match self.eval(*body.clone()) {
                                Ok(_) => {},
                                Err(s) => match s {
                                    State::Continue => continue,
                                    State::Break => break,
                                    _ => return Err(s)
                                }
                            };
                        }
                        self.exit_scope();
                    }
                    _ => return Err(State::Error(RuntimeError::TypeMismatch))
                }
                Ok(Value::None)
            }
            Node::LetStatement { name, expr } => {
                self.error_node = node;
                let val = self.eval(*expr)?;
                if !self.current_scope.env.contains_key(&name) {
                    self.current_scope.env.insert(name, val); Ok(Value::None)
                } else { 
                    Err(State::Error(RuntimeError::AlreadyDefinedVarible)) 
                }
            },
            Node::Return(expr) => Err(State::Return(self.eval(*expr)?)),
            Node::IfExpression { cond, tru, fals } => {
                self.error_node = node;
                if let Value::Bool(t) = self.eval(*cond)? {
                    if t { Ok(self.eval(*tru)?) } 
                    else { Ok(self.eval(*fals)?) }
                } else { 
                    Err(State::Error(RuntimeError::TypeMismatch)) 
                }
            }
            Node::RecordDeclaration { name, members } => {
                self.error_node = node;
                if self.current_scope.env.contains_key(&name) {
                    return Err(State::Error(RuntimeError::AlreadyDefinedVarible));
                }
                self.add_record(Record {name, members});
                Ok(Value::None)                
            },
            Node::FunctionDeclaration { name, args, body } => {
                let closure = if let Some(_) = self.current_scope.upper {
                    Some(self.current_scope.env.clone())
                } else { None };
                if !name.is_empty() {
                    self.add_function(Function { name, args, body: *body, closure });
                    Ok(Value::None)
                } else {
                    let fname ="lambda".into();
                    self.add_function(Function { name: fname, args, body: *body, closure });
                    Ok(Value::Function(self.func_id - 1))
                }
            },
            Node::IfStatement { expr, body, els } => {
                self.error_node = node;
                if let Value::Bool(t) = self.eval(*expr)? {
                    if t { Ok(self.eval(*body)?) } 
                    else { Ok(self.eval(*els)?) }
                } else { 
                    Err(State::Error(RuntimeError::TypeMismatch)) 
                }
            }
            Node::WhileStatement { expr, body } => {
                self.error_node = node;
                loop {
                    match self.eval(*expr.clone())? {
                        Value::Bool(t) => if t {
                            match self.eval(*body.clone()) {
                                Ok(_) => {},
                                Err(s) => match s {
                                    State::Continue => continue,
                                    State::Break => break Ok(Value::None),
                                    _ => return Err(s)
                                }
                            };
                        } else { return Ok(Value::None) }
                        _ => return Err(State::Error(RuntimeError::TypeMismatch))
                    }
                }
            },
            Node::BinaryExpression { op, lhs, rhs } => {
                self.error_node = node;
                if op.as_str() == "=" {
                    let right_value = self.eval(*rhs)?;
                    return match *lhs {
                        Node::Identifier(var) => 
                            match self.current_scope.assign(&var, &right_value) {
                                Ok(()) => Ok(right_value),
                                Err(err) => Err(State::Error(err))
                            },
                        Node::BinaryExpression { op, lhs, rhs } => {
                            let llhs_value = self.eval(*lhs)?;
                            if op.as_str() == "." {
                                return match (llhs_value, *rhs) {
                                    (Value::Instance(_, id), Node::Identifier(member)) => {
                                        let ins = self.get_instance_mut(&id);
                                        let attr = ins.get_mut(&member).unwrap();
                                        *attr = right_value.clone();
                                        Ok(right_value)
                                    }
                                    (Value::Module(id), Node::Identifier(member)) => {
                                        let module = self.get_module_mut(&id);
                                        match module.scope.assign(&member, &right_value) {
                                            Ok(()) => Ok(right_value),
                                            Err(err) => Err(State::Error(err))
                                        }
                                    },
                                    _ => Err(State::Error(RuntimeError::MemberAccessError))
                                }                            }
                            let lrhs_value = self.eval(*rhs)?;
                            if op.as_str() == "[" {
                                return match (llhs_value, lrhs_value.clone()) {
                                    (Value::List(id), Value::Int(index)) => {
                                        let list = self.get_list_mut(&id);
                                        if index as usize >= list.len() {return Err(State::Error(RuntimeError::IndexOutOfRange));}
                                        let _ = std::mem::replace(&mut list[index as usize], right_value.clone());
                                        Ok(right_value)
                                    },
                                    (Value::Map(id), _) => {
                                        let key = lrhs_value.to_keyvalue();
                                        if key.is_none() {
                                            return Err(State::Error(RuntimeError::KeyError));
                                        }
                                        let map = self.get_map_mut(&id);
                                        map.insert(key.unwrap(), right_value.clone());
                                        Ok(right_value)
                                    }
                                    _ => Err(State::Error(RuntimeError::TypeMismatch))
                                }
                            }
                            Err(State::Error(RuntimeError::AssignmentError))
                        }
                        _ => Err(State::Error(RuntimeError::AssignmentError))
                    }
                }
                if op.as_str() == "." {
                    let left_value = self.eval(*lhs)?;
                    return match (left_value, *rhs.clone()) {                            
                        (Value::Instance(_, id), Node::Identifier(member)) => {
                            let ins = self.get_instance(&id);
                            match ins.get(&member) {
                                Some(val) => Ok(val.clone()),
                                None => Err(State::Error(RuntimeError::MemberAccessError)) 
                            }
                        }
                        (Value::Module(id), Node::Identifier(member)) => {
                            let module = self.get_module(&id);
                            let res = module.scope.resolve(&member);
                            if res.is_none() {
                                Err(State::Error(RuntimeError::MemberAccessError))
                            } else { Ok(res.unwrap()) }
                        },
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                }
                let left_value = self.eval(*lhs)?;
                let right_value = self.eval(*rhs)?;
                match op.as_str() {
                    "+" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 + b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    } 
                    "-" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 - b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                    "*" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 * b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                    "/" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) =>
                            if b != 0 { Ok(Value::Int(a / b)) } 
                            else { Err(State::Error(RuntimeError::DivisionByZero)) }
                        (Value::Float(a), Value::Float(b)) =>
                            if b != 0.0 { Ok(Value::Float(a / b)) } 
                            else { Err(State::Error(RuntimeError::DivisionByZero)) }
                        (Value::Float(a), Value::Int(b)) => 
                            if b != 0 { Ok(Value::Float(a / b as f32)) } 
                            else { Err(State::Error(RuntimeError::DivisionByZero)) }
                        (Value::Int(a), Value::Float(b)) => 
                            if b != 0.0 { Ok(Value::Float(a as f32 / b)) } 
                            else { Err(State::Error(RuntimeError::DivisionByZero)) }
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                    "%" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                    "<=" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a <= b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 <= b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    },
                    ">=" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a >= b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 >= b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    },
                    "<" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a < b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((a as f32) < b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    },
                    ">" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a > b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 > b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    },
                    "!=" => if let Value::Bool(b) = left_value.equality(right_value, self)? {
                            return Ok(Value::Bool(!b));
                        } else { unreachable!() }
                    "==" => left_value.equality(right_value, self),
                    "||" => match (left_value, right_value) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    },
                    "&&" => match (left_value, right_value) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    },
                    ".." => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => 
                            if a < b {
                                Ok(Value::Range(a, b))
                            } else { Err(State::Error(RuntimeError::BadRange)) }
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    },
                    "::" => match (left_value.clone(), right_value) {
                        (_, Value::Type(typ)) => Ok(Value::Bool(left_value.get_type() == typ)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    },
                    "[" => match (left_value, right_value.clone()) {
                        (Value::List(id), Value::Int(index)) => {
                            let list = self.get_list(&id);
                            if index as usize >= list.len() {return Err(State::Error(RuntimeError::IndexOutOfRange));}
                            Ok(self.get_list(&id)[index as usize].clone())
                        },
                        (Value::String(string), Value::Int(index)) => {
                            let res = string.chars().nth(index as usize);
                            if res.is_none() { Err(State::Error(RuntimeError::IndexOutOfRange)) }
                            else { Ok(Value::String(res.unwrap().to_string())) }
                        },
                        (Value::Map(id), _) => {
                            let key = right_value.to_keyvalue();
                            if key.is_none() {
                                return Err(State::Error(RuntimeError::KeyError));
                            }
                            let map = self.get_map(&id);
                            let o = &key.unwrap();
                            if !map.contains_key(o) {
                                Err(State::Error(RuntimeError::KeyError))
                            } else {
                                Ok(map[o].clone())
                            }
                        }
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                    "|>" => right_value.apply(self, &[left_value]), 
                    _ => unreachable!() 
                }
            },
            Node::IntegerLiteral(i) => Ok(Value::Int(i)),
            Node::FloatLiteral(f)   => Ok(Value::Float(f)),
            Node::StringLiteral(s)  => Ok(Value::String(s)),
            Node::True              => Ok(Value::Bool(true)),
            Node::False             => Ok(Value::Bool(false)),
            Node::Nothing           => Ok(Value::Nothing),
            Node::None              => Ok(Value::None),
            Node::Break             => Err(State::Break),
            Node::Continue          => Err(State::Continue),
            Node::Group(expr)       => self.eval(*expr),
            Node::ListLiteral(nodes) => {
                let values = self.collect_values(nodes)?;
                Ok(self.add_list(values))
            },
            Node::Identifier(s) => {
                self.error_node = node;
                match self.current_scope.resolve(&s) {
                    Some(value) => Ok(value),
                    None => match self.globals.get(&s) {
                        Some(val) => Ok(val.clone()),
                        None => Err(State::Error(RuntimeError::UndefinedVariable))
                    }
                }
            },
            Node::UnaryExpression { op, operand } => {
                self.error_node = node;
                let operand_value = self.eval(*operand)?;
                match op.as_str() {
                    "+" => match operand_value {
                        Value::Int(a)   => Ok(Value::Int(a)),
                        Value::Float(a) => Ok(Value::Float(a)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                    "-" => match operand_value {
                        Value::Int(a)   => Ok(Value::Int(-a)),
                        Value::Float(a) => Ok(Value::Float(-a)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                    "!" => match operand_value {
                        Value::Bool(a) => Ok(Value::Bool(!a)),
                        _ => Err(State::Error(RuntimeError::TypeMismatch))
                    }
                    _ => unreachable!()
                }
            },
            Node::Application { fun, args } => {
                let values = self.collect_values(args)?;
                self.eval(*fun)?.apply(self, &values)
            },
        }
    }
    
}