use std::collections::HashMap;
use std::process::exit;
use std::vec;
use std::fs;

use crate::core_module;
use crate::math_module;

use crate::value::{ Value, Type };
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

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: Node,
    pub closure: Option<HashMap<String, Value>>
}

impl Function {
    pub fn call(&self, func_id: u64, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
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
            Value::Function(func_id)
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

    pub fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, RuntimeError> {
        if self.arity != args.len() {
            return Err(RuntimeError::ArgNumMismatch);
        }
        (self.body)(interpreter, args)
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
    classes      : HashMap<u64, ClassDef>,
    modules      : HashMap<u64, Module>,
    functions    : HashMap<u64, Function>,
    nfunctions   : HashMap<u64, NativeFunction>,

    ins_id       : u64,
    list_id      : u64,
    mod_id       : u64,
    class_id     : u64,
    func_id      : u64,
    nfunc_id     : u64,
    lambda_id    : u64,
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
            lambda_id: 0,
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
        self.classes.get(id).expect("No Class with this id.")
    }

    pub fn get_module(&self, id: &u64) -> &Module {
        self.modules.get(id).expect("No module with this id.")
    }

    pub fn get_instance(&self, id: &u64) -> &HashMap<String, Value> {
        self.instances.get(id).expect("No Instance with this id.")
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
                self.eval(*block)?;
                let scope = self.current_scope.clone();
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
                // Maybe allow shadowing?
                if !self.current_scope.env.contains_key(&name) {
                    self.current_scope.env.insert(name, val); Ok(Value::None)
                } else { Err(RuntimeError::AlreadyDefinedVarible) }
            },
            Node::Return(expr) => {
                self.return_val = self.eval(*expr)?;
                Ok(Value::None)
            },
            Node::Class { name, members } => {
                if self.current_scope.env.contains_key(&name) {
                    return Err(RuntimeError::AlreadyDefinedVarible);
                }
                let id = self.class_id;
                self.classes.insert(id, ClassDef { name: name.clone(), members });
                self.class_id += 1;
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
                    self.add_function(Function { name, args, body: *body, closure });
                    Ok(Value::None)
                } else {
                    let fname ="lambda ".to_string() + &self.lambda_id.to_string();
                    self.lambda_id += 1;
                    self.add_function(Function { name: fname, args, body: *body, closure });
                    Ok(Value::Function(self.func_id - 1))
                }
            },
            Node::If { expr, body, els } => {
                match self.eval(*expr)? {
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
                        match self.eval(*expr)? {
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
                while self.return_val == Value::None || self.break_flag {
                    match self.eval(*expr.clone())? {
                        Value::Bool(t) => {
                            if t {
                                assert!(self.eval(*body.clone()).unwrap() == Value::None, "While didn't returned None");
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
                let lhs_value = self.eval(*lhs.clone())?;
                let mut rhs_value = Value::None;
                if op.as_str() != "." {
                    rhs_value = self.eval(*rhs.clone())?;
                }
                // Refactor, hopefully
                match op.as_str() {
                    "+" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 + b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    } 
                    "-" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 - b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "*" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 * b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "/" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => {
                            if b != 0 { Ok(Value::Int(a / b)) } 
                            else { Err(RuntimeError::DivisionByZero) }
                        },
                        (Value::Float(a), Value::Float(b)) => {
                            if b != 0.0 { Ok(Value::Float(a / b)) } 
                            else { Err(RuntimeError::DivisionByZero) }
                        },
                        (Value::Float(a), Value::Int(b)) => {
                            if b != 0 { Ok(Value::Float(a / b as f32)) } 
                            else { Err(RuntimeError::DivisionByZero) }
                        },
                        (Value::Int(a), Value::Float(b)) => {
                            if b != 0.0 { Ok(Value::Float(a as f32 / b)) } 
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
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a <= b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 <= b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    ">=" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a >= b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 >= b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "<" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a < b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((a as f32) < b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    ">" => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a > b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 > b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "!=" => if let Value::Bool(b) = self.equality(lhs_value, rhs_value)? {
                                return Ok(Value::Bool(!b));
                            } else { unreachable!() }
                    "==" => self.equality(lhs_value, rhs_value),
                    "||" => match (lhs_value, rhs_value) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    "&&" => match (lhs_value, rhs_value) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                        _ => Err(RuntimeError::TypeMismatch)
                    },
                    ".." => match (lhs_value, rhs_value) {
                        (Value::Int(a), Value::Int(b)) => if a < b {
                            Ok(Value::Range(a, b))
                        } else { Err(RuntimeError::BadRange) }
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
                                        module.scope.assign(&member, &rhs_value)?;
                                        Ok(rhs_value)
                                    },
                                    _ => Err(RuntimeError::MemberAccessError)
                                }
                                "[" => match (llhs_value, lrhs_value) {
                                    (Value::List(id), Value::Int(index)) => {
                                        let list = self.lists.get_mut(&id)
                                            .expect("List couldn't find.");
                                        if index as usize >= list.len() {return Err(RuntimeError::IndexOutOfRange);}
                                        let _ = std::mem::replace(&mut list[index as usize], rhs_value.clone());
                                        Ok(rhs_value)
                                    }
                                    _ => Err(RuntimeError::TypeMismatch)
                                }
                                _ => Err(RuntimeError::AssignmentError)
                            }
                        }
                        _ => Err(RuntimeError::AssignmentError)
                    },
                    "." => match (lhs_value, *rhs.clone()) {                            
                        (Value::Instance(_, id), Node::Identifier(member)) => {
                            let ins = &self.instances[&id];
                            match ins.get(&member) {
                                Some(val) => Ok(val.clone()),
                                None => Err(RuntimeError::MemberAccessError) 
                            }
                        }
                        (Value::Module(id), Node::Identifier(member)) => {
                            let module = self.modules.get(&id)
                                .expect("Module couldn't find.");
                            let res = module.scope.resolve(&member);
                            if res.is_none() {
                                Err(RuntimeError::MemberAccessError)
                            } else { Ok(res.unwrap()) }
                        },
                        _ => Err(RuntimeError::TypeMismatch)
                    }
                    "[" => match (lhs_value, rhs_value) {
                        (Value::List(id), Value::Int(index)) => {
                            let list = self.get_list(&id);
                            if index as usize >= list.len() {return Err(RuntimeError::IndexOutOfRange);}
                            Ok(self.get_list(&id)[index as usize].clone())
                        },
                        (Value::String(string), Value::Int(index)) => {
                            let res = string.chars().nth(index as usize);
                            if res.is_none() { Err(RuntimeError::IndexOutOfRange) }
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
                let mut values: Vec<Value> = vec![];
                for node in nodes {
                    values.push(self.eval(node).expect("Expected Value"));
                }
                
                let id = self.list_id;
                self.lists.insert(id, values);
                self.list_id += 1;
                Ok(Value::List(id))
            },
            Node::Identifier(s) => { 
                match self.current_scope.resolve(&s){
                    Some(value) => Ok(value),
                    None => {
                        match self.globals.get(&s) {
                            Some(val) => Ok(val.clone()),
                            None => Err(RuntimeError::UndefinedVariable)
                        }
                    }
                }
            },
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
                        let mut values = vec![];
                        for arg in node_args {
                            values.push(self.eval(arg)?);
                        }
                        nfunction.call(self, &values)
                    }
                    Value::Function(id) => {
                        let function = self.get_function(&id).clone(); 
                        //let values: Vec<_> = 
                        //    node_args.iter().map(|arg| self.eval(*arg)).collect();
                        let mut values = vec![];
                        for arg in node_args {
                            values.push(self.eval(arg)?);
                        }
                        function.call(id, self, &values)
                    }
                    Value::Type(typ) => {
                        let class_id = match typ {
                            Type::Custom(id) => id,
                            _ => {unreachable!()}
                        };
                        let class = self.classes.get(&class_id).unwrap().clone();
                        if class.members.len() != node_args.len() {
                            return Err(RuntimeError::ArgNumMismatch);
                        }
                        let mut values = HashMap::new();
                        for i in 0..class.members.len() {
                            values.insert(class.members[i].clone(), self.eval(node_args[i].clone())?);
                        }
                        let id = self.ins_id;
                        self.instances.insert(id, values);
                        self.ins_id += 1;
                        Ok(Value::Instance(class_id, id))
                    },
                    _ => Err(RuntimeError::TypeMismatch)
                }
            },
        }
    }
    
}