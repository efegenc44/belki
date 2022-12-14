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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    env: HashMap<String, Value>,
    upper: Option<Box<Scope>>
}

pub struct Module {
    pub name: String,
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
pub struct ClassDef {
    pub name: String,
    pub members: Vec<String>,
}

type Env = HashMap<String, Value>; 

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub id  : u64,
    pub args: Vec<String>,
    pub body: Node,
    pub closure: Option<Env>
}
#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub body: fn(&mut Interpreter, &[Value]) -> Result<Value, RuntimeError>,
}

impl NativeFunction {
    pub fn new(name: String, arity: usize, body: fn(&mut Interpreter, &[Value]) -> Result<Value, RuntimeError>) -> NativeFunction {
        NativeFunction { name, arity, body }
    }
}

pub trait Applicable {
    fn apply(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError>;
}

impl Applicable for Function {
    fn apply(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        if self.args.len() != args.len() {
            return Err(RuntimeError::ArgNumMismatch);
        }
        interpreter.enter_scope();
        // -- HACK for prevent from shadowing => Capture only needed variables
        if let Some(scope) = &self.closure {
            interpreter.current_scope.env.extend(scope.clone());    
        }
        // --
        // self-reference 
        interpreter.current_scope.env.insert(
            self.name.clone(), 
            Value::Function(self.id)
        );
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

impl Applicable for NativeFunction {
    fn apply(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        if self.arity != args.len() {
            return Err(RuntimeError::ArgNumMismatch);
        }
        (self.body)(interpreter, args)
    }
}

impl Applicable for Type {
    fn apply(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        let class_id = match self {
            Type::Custom(id) => id,
            _ => {unreachable!()}
        };
        let class = interpreter.get_classdef(&class_id).clone();
        if class.members.len() != args.len() {
            return Err(RuntimeError::ArgNumMismatch);
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
}

pub struct Interpreter {
    current_scope: Scope,
    return_val   : Value,
    break_flag   : bool,      
    continue_flag: bool,      
    globals      : HashMap<String, Value>,
    
    repl         : bool,

    instances    : HashMap<u64, HashMap<String, Value>>,
    lists        : HashMap<u64, Vec<Value>>,
    types        : HashMap<u64, ClassDef>,
    modules      : HashMap<u64, Module>,
    functions    : HashMap<u64, Function>,
    nfunctions   : HashMap<u64, NativeFunction>,
    maps         : HashMap<u64, HashMap<KeyValue, Value>>,

    ins_id       : u64,
    list_id      : u64,
    mod_id       : u64,
    type_id      : u64,
    func_id      : u64,
    nfunc_id     : u64,
    lambda_id    : u64,
    map_id       : u64,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { 
            current_scope: Scope::new(),
            return_val   : Value::None,
            break_flag   : false,
            continue_flag: false,
            globals      : HashMap::new(),

            repl: false,

            instances : HashMap::new(),
            lists     : HashMap::new(),
            types     : HashMap::new(),
            modules   : HashMap::new(),
            functions : HashMap::new(),
            nfunctions: HashMap::new(),
            maps      : HashMap::new(),

            ins_id   : 0,
            list_id  : 0,
            mod_id   : 0,
            type_id  : 0,
            func_id  : 0,
            nfunc_id : 0,
            lambda_id: 0,
            map_id   : 0,
        }
    }
    
    pub fn repl_mode(&mut self) {
        self.repl = true;
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

    pub fn get_classdef(&self, id: &u64) -> &ClassDef {
        self.types.get(id).expect("No Class with this id.")
    }

    pub fn get_module(&self, id: &u64) -> &Module {
        self.modules.get(id).expect("No module with this id.")
    }

    pub fn get_instance(&self, id: &u64) -> &HashMap<String, Value> {
        self.instances.get(id).expect("No Instance with this id.")
    }

    pub fn get_map(&self, id: &u64) -> &HashMap<KeyValue, Value> {
        self.maps.get(id).expect("No Map with this id.")
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
                module.scope.env.insert(func.name.clone(), Value::NativeFunction(id));
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
        self.modules.insert(id, Module {name: name.clone(), scope: scope.clone()});
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

    pub fn add_type(&mut self, typ: ClassDef) {
        let id = self.type_id;
        self.types.insert(id, typ.clone());
        self.type_id += 1;
        self.current_scope.env.insert(
            typ.name,
            Value::Type(Type::Custom(id))
        );
    }

    pub fn add_global_variable(&mut self, name: String, value: Value) {
        self.globals.insert(name, value);
    }

    fn equality(&mut self, lhs_value: Value, rhs_value: Value) -> Result<Value, RuntimeError> {
        match (lhs_value.clone(), rhs_value.clone()) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a == b as f32)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 == b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
            (Value::List(ida), Value::List(idb)) => Ok(Value::Bool(self.get_list(&ida) == self.get_list(&idb))),
            (Value::Type(type1), Value::Type(type2)) => Ok(Value::Bool(type1 == type2)),
            _ => {
                if std::mem::discriminant(&lhs_value) != 
                    std::mem::discriminant(&rhs_value) {
                        return Ok(Value::Bool(false));
                    }
                Err(RuntimeError::TypeMismatch)
            }
        }
    }

    fn collect_values(&mut self, nodes: Vec<Node>) -> Result<Vec<Value>, RuntimeError> {
        let mut values = vec![];
        for node in nodes { 
            values.push(self.eval(node)?); 
        }; Ok(values)
    }

    pub fn eval(&mut self, node: Node) -> Result<Value, RuntimeError> {
        match node {
            Node::Program(statements) => {
                for statement in statements {
                    let val = self.eval(statement)?;
                    if self.repl && val != Value::None {
                        println!("{}", val.get_string(self));
                    }
                } Ok(Value::None)
            },
            Node::Block(statements) => {
                self.enter_scope();
                for statement in statements {
                    if self.return_val != Value::None ||
                        self.break_flag || 
                        self.continue_flag 
                    {
                        break
                    }
                    self.eval(statement)?;
                }
                self.exit_scope(); 
                Ok(Value::None)
            },
            Node::Block2(statements) => {
                for statement in statements {
                    if self.return_val != Value::None ||
                        self.break_flag || 
                        self.continue_flag 
                    {
                        break
                    }
                    self.eval(statement)?;
                }
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
            Node::Module(name, block) => {
                self.enter_scope();
                let up = self.current_scope.upper.clone();
                self.current_scope.upper = None;
                self.enter_scope();
                self.eval(*block)?;
                let scope = self.current_scope.clone();
                self.exit_scope();
                self.current_scope.upper = up;
                self.exit_scope();
                
                let id = self.mod_id;
                self.modules.insert(id, Module { name: name.clone(), scope });
                self.mod_id += 1;
                self.current_scope.env.insert(
                    name, 
                    Value::Module(id)
                );
                Ok(Value::None)
            }
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
                let cs = self.current_scope.clone();
                self.current_scope = Scope::new();
                self.eval(node)?;
                
                let name = path
                    .split("/").collect::<Vec<_>>().last().unwrap().to_string()    
                    .split(".").collect::<Vec<_>>()[0].to_string();
                
                let id = self.mod_id;
                self.modules.insert(id, Module { name: name.clone(), scope: self.current_scope.clone()});
                self.mod_id += 1;
                self.current_scope = cs;
                self.current_scope.env.insert(
                    name, 
                    Value::Module(id)
                );
                Ok(Value::None)
            },
            Node::MapLit(map) => {
                let mut hmap = HashMap::new();
                for (k, v) in map {
                    let key = self.eval(k)?.to_keyvalue(); 
                    match key {
                        Some(_) => {hmap.insert(key.unwrap(), self.eval(v)?);},
                        None => {return Err(RuntimeError::HashError);}
                    }
                }
                let id = self.map_id;
                self.map_id += 1;
                self.maps.insert(id, hmap);
                Ok(Value::Map(id))
            }
            Node::For { var, iter, body } => {
                match self.eval(*iter)? {
                    Value::List(idx) => {
                        self.enter_scope();
                        let list = self.get_list(&idx).clone();
                        for i in list {
                            if self.return_val != Value::None || self.break_flag {
                                break
                            }
                            self.current_scope.env.insert(var.clone(), i.clone());
                            self.eval(*body.clone())?;
                            if self.continue_flag {
                                self.continue_flag = false;
                                continue;
                            }
                        }
                        self.break_flag = false;
                        self.exit_scope();
                    },
                    Value::String(string) => {
                        self.enter_scope();
                        for (_, i) in string.chars().enumerate() {
                            if self.return_val != Value::None || self.break_flag {
                                break
                            }
                            self.current_scope.env.insert(var.clone(), Value::String(i.into()));
                            self.eval(*body.clone())?;
                            if self.continue_flag {
                                self.continue_flag = false;
                                continue;
                            }
                        }
                        self.break_flag = false;
                        self.exit_scope();
                    },
                    Value::Range(de, a) => {
                        self.enter_scope();
                        for i in de..a {
                            if self.return_val != Value::None || self.break_flag {
                                break
                            }
                            self.current_scope.env.insert(var.clone(), Value::Int(i));
                            self.eval(*body.clone())?;
                            if self.continue_flag {
                                self.continue_flag = false;
                                continue;
                            }
                        }
                        self.break_flag = false;
                        self.exit_scope();
                    }
                    _ => {return Err(RuntimeError::TypeMismatch);}
                }
                Ok(Value::None)
            }
            Node::Let { name, expr } => {
                let val = self.eval(*expr)?;
                if !self.current_scope.env.contains_key(&name) {
                    self.current_scope.env.insert(name, val); Ok(Value::None)
                } else { 
                    Err(RuntimeError::AlreadyDefinedVarible) 
                }
            },
            Node::Return(expr) => {
                self.return_val = self.eval(*expr)?;
                Ok(Value::None)
            },
            Node::IfExpr { cond, tru, fals } => if let Value::Bool(t) = self.eval(*cond)? {
                    if t { Ok(self.eval(*tru)?) } 
                    else { Ok(self.eval(*fals)?) }
                } else { 
                    Err(RuntimeError::TypeMismatch) 
                }
            Node::Class { name, members } => {
                if self.current_scope.env.contains_key(&name) {
                    return Err(RuntimeError::AlreadyDefinedVarible);
                }
                let id = self.type_id;
                self.types.insert(id, ClassDef { name: name.clone(), members });
                self.type_id += 1;
                self.current_scope.env.insert(
                    name,
                    Value::Type(Type::Custom(id))
                );
                Ok(Value::None)                
            },
            Node::Fun { name, args, body } => {
                let closure = if let Some(_) = self.current_scope.upper {
                    Some(self.current_scope.env.clone())
                } else { None };
                if !name.is_empty() {
                    self.add_function(Function { name, id: self.func_id, args, body: *body, closure });
                    Ok(Value::None)
                } else {
                    let fname ="lambda ".into();
                    self.lambda_id += 1;
                    self.add_function(Function { name: fname, id: self.func_id, args, body: *body, closure });
                    Ok(Value::Function(self.func_id - 1))
                }
            },
            Node::If { expr, body, els } => if let Value::Bool(t) = self.eval(*expr)? {
                if t { Ok(self.eval(*body)?) } 
                else { Ok(self.eval(*els)?) }
            } else { 
                Err(RuntimeError::TypeMismatch) 
            }
            Node::While { expr, body } => {
                while self.return_val == Value::None || self.break_flag {
                    match self.eval(*expr.clone())? {
                        Value::Bool(t) => {
                            if t {
                                self.eval(*body.clone())?;
                                if self.continue_flag {
                                    self.continue_flag = false;
                                    continue;
                                }
                            } else { return Ok(Value::None) }
                        },
                        _ => return Err(RuntimeError::TypeMismatch)
                    }
                }
                self.break_flag = false;
                Ok(Value::None)
            },
            Node::Binary { op, lhs, rhs } => {
                let mut left_value = Value::None;
                if op.as_str() != "=" {
                    left_value = self.eval(*lhs.clone())?;
                }
                let mut right_value = Value::None;
                if op.as_str() != "." {
                    right_value = self.eval(*rhs.clone())?;
                }
                match op.as_str() {
                    "+" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 + b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    } 
                    "-" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 - b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "*" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 * b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "/" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) =>
                            if b != 0 { Ok(Value::Int(a / b)) } 
                            else { Err(RuntimeError::DivisionByZero) }
                        (Value::Float(a), Value::Float(b)) =>
                            if b != 0.0 { Ok(Value::Float(a / b)) } 
                            else { Err(RuntimeError::DivisionByZero) }
                        (Value::Float(a), Value::Int(b)) => 
                            if b != 0 { Ok(Value::Float(a / b as f32)) } 
                            else { Err(RuntimeError::DivisionByZero) }
                        (Value::Int(a), Value::Float(b)) => 
                            if b != 0.0 { Ok(Value::Float(a as f32 / b)) } 
                            else { Err(RuntimeError::DivisionByZero) }
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "%" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "<=" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a <= b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 <= b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    ">=" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a >= b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 >= b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "<" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a < b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((a as f32) < b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    ">" => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a > b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 > b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "!=" => if let Value::Bool(b) = self.equality(left_value, right_value)? {
                                return Ok(Value::Bool(!b));
                            } else { unreachable!() }
                    "==" => self.equality(left_value, right_value),
                    "||" => match (left_value, right_value) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "&&" => match (left_value, right_value) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    ".." => match (left_value, right_value) {
                        (Value::Int(a), Value::Int(b)) => 
                            if a < b {
                                Ok(Value::Range(a, b))
                            } else { Err(RuntimeError::BadRange) }
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "::" => match (left_value.clone(), right_value) {
                        (_, Value::Type(typ)) => Ok(Value::Bool(left_value.get_type() == typ)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "=" => match *lhs {
                        Node::Identifier(var) => {
                            self.current_scope.assign(&var, &right_value)?;
                            Ok(right_value)
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
                                        *attr = right_value.clone();
                                        Ok(right_value)
                                    }
                                    (Value::Module(id), Node::Identifier(member)) => {
                                        let module = self.modules.get_mut(&id)
                                            .expect("Module couldn't find.");
                                        module.scope.assign(&member, &right_value)?;
                                        Ok(right_value)
                                    },
                                    _ => Err(RuntimeError::MemberAccessError)
                                }
                                "[" => match (llhs_value, lrhs_value.clone()) {
                                    (Value::List(id), Value::Int(index)) => {
                                        let list = self.lists.get_mut(&id)
                                            .expect("List couldn't find.");
                                        if index as usize >= list.len() {return Err(RuntimeError::IndexOutOfRange);}
                                        let _ = std::mem::replace(&mut list[index as usize], right_value.clone());
                                        Ok(right_value)
                                    },
                                    (Value::Map(id), _) => {
                                        let key = lrhs_value.to_keyvalue();
                                        if key.is_none() {
                                            return Err(RuntimeError::KeyError);
                                        }
                                        let map = self.maps.get_mut(&id)
                                            .expect("Map couldn't find.");
                                        map.insert(key.unwrap(), right_value.clone());
                                        Ok(right_value)
                                    }
                                    _ => Err(RuntimeError::TypeMismatch)
                                }
                                _ => Err(RuntimeError::AssignmentError)
                            }
                        }
                        _ => Err(RuntimeError::AssignmentError)
                    },
                    "." => match (left_value, *rhs.clone()) {                            
                        (Value::Instance(_, id), Node::Identifier(member)) => {
                            let ins = self.get_instance(&id);
                            match ins.get(&member) {
                                Some(val) => Ok(val.clone()),
                                None => Err(RuntimeError::MemberAccessError) 
                            }
                        }
                        (Value::Module(id), Node::Identifier(member)) => {
                            let module = self.get_module(&id);
                            let res = module.scope.resolve(&member);
                            if res.is_none() {
                                Err(RuntimeError::MemberAccessError)
                            } else { Ok(res.unwrap()) }
                        },
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "[" => match (left_value, right_value.clone()) {
                        (Value::List(id), Value::Int(index)) => {
                            let list = self.get_list(&id);
                            if index as usize >= list.len() {return Err(RuntimeError::IndexOutOfRange);}
                            Ok(self.get_list(&id)[index as usize].clone())
                        },
                        (Value::String(string), Value::Int(index)) => {
                            let res = string.chars().nth(index as usize);
                            if res.is_none() { Err(RuntimeError::IndexOutOfRange) }
                            else { Ok(Value::String(res.unwrap().to_string())) }
                        },
                        (Value::Map(id), _) => {
                            let key = right_value.to_keyvalue();
                            if key.is_none() {
                                return Err(RuntimeError::KeyError);
                            }
                            let map = self.get_map(&id);
                            let o = &key.unwrap();
                            if !map.contains_key(o) {
                                Err(RuntimeError::KeyError)
                            } else {
                                Ok(map[o].clone())
                            }
                        }
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "|>" => match right_value {
                        Value::Function(id)       => self.get_function(&id).clone().apply(self, &[left_value]),
                        Value::NativeFunction(id) => self.get_nfunction(&id).clone().apply(self, &[left_value]),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    _ => unreachable!() 
                }
            },
            Node::Integer(i) => Ok(Value::Int(i)),
            Node::Float(f)   => Ok(Value::Float(f)),
            Node::String(s)  => Ok(Value::String(s)),
            Node::True       => Ok(Value::Bool(true)),
            Node::False      => Ok(Value::Bool(false)),
            Node::Nothing    => Ok(Value::Nothing),
            Node::None       => Ok(Value::None),
            Node::Break      => {
                self.break_flag = true;
                Ok(Value::None)
            },
            Node::Continue   => {
                self.continue_flag = true;
                Ok(Value::None)
            },
            Node::Group(expr) => self.eval(*expr),
            Node::List(nodes) => {
                let values = self.collect_values(nodes)?;
                self.lists.insert(self.list_id, values);
                self.list_id += 1;
                Ok(Value::List(self.list_id - 1))
            },
            Node::Identifier(s) => match self.current_scope.resolve(&s) {
                Some(value) => Ok(value),
                None => match self.globals.get(&s) {
                    Some(val) => Ok(val.clone()),
                    None => Err(RuntimeError::UndefinedVariable)
                }
            },
            Node::Unary { op, operand } => {
                let operand_value = self.eval(*operand)?;
                match op.as_str() {
                    "+" => match operand_value {
                        Value::Int(a)   => Ok(Value::Int(a)),
                        Value::Float(a) => Ok(Value::Float(a)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "-" => match operand_value {
                        Value::Int(a)   => Ok(Value::Int(-a)),
                        Value::Float(a) => Ok(Value::Float(-a)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "!" => match operand_value {
                        Value::Bool(a) => Ok(Value::Bool(!a)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    _ => unreachable!()
                }
            },
            Node::Application { fun, args } => {
                let applicable_value = self.eval(*fun)?;
                let values = self.collect_values(args)?;
                match applicable_value {
                    Value::NativeFunction(id) => self.get_nfunction(&id).clone().apply(self, &values),
                    Value::Function(id)       => self.get_function(&id).clone().apply(self, &values),
                    Value::Type(typ)          => typ.apply(self, &values),
                    _ => Err(RuntimeError::TypeMismatch)
                }
            },
        }
    }
    
}