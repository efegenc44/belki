use std::collections::HashMap;
use std::process::exit;
use std::vec;
use std::fs;

use crate::core_module;
use crate::math_module;

use crate::value::{ Value, Type, KeyValue };
use crate::token::Location;
use crate::ast::Node;
use crate::lexer::Lexer;
use crate::parser::Parser;

type Symbol      = String;
type List        = Vec<Value>;
type EvalResult  = Result<Value, State>;
type Environment = HashMap<String, Value>; 
type Map         = HashMap<KeyValue, Value>; 

pub type RuntimeError = String;

pub fn runtime_error(msg: String, loc: Location) -> RuntimeError {
    format!("\n  An error occured at runtime:\n    {}\n    {}\n", loc, msg)  
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

    fn assign(&mut self, var: &String, val: &Value, loc: Location) -> Result<(), RuntimeError> {
        match self.env.get_mut(var) {
            Some(value) => {
                *value = val.clone(); Ok(())
            }
            None => match &mut self.upper {
                Some(scope) => scope.assign(var, val, loc),
                None => Err(runtime_error(
                    format!("Undefined Variable '{}'", var), loc
                ))
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
    pub body: fn(&mut Interpreter, &[Value], &Location) -> EvalResult,
}

impl NativeFunction {
    pub fn new(name: String, arity: usize, body: fn(&mut Interpreter, &[Value], &Location) -> EvalResult) -> NativeFunction {
        NativeFunction { name, arity, body }
    }
}

impl Value {
    pub fn apply(&self, interpreter: &mut Interpreter, args: &[Value], loc: &Location) -> EvalResult {
        match self {
            Value::Function(id) => {
                let function = interpreter.get_function(&id).clone();
                if function.args.len() != args.len() {
                    return Err(State::Error(runtime_error(
                        format!("'{}' function takes {} arguments but {} given", function.name, function.args.len(), args.len()), loc.to_owned()
                    )));
                }
                interpreter.enter_scope();

                if let Some(scope) = &function.closure {
                    interpreter.current_scope.env.extend(scope.clone());    
                }

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
                    return Err(State::Error(runtime_error(
                        format!("'{}' function takes {} arguments but {} given", nfunction.name, nfunction.arity, args.len()), loc.to_owned()
                    )));
                }
                (nfunction.body)(interpreter, args, loc)
            },
            Value::Type(typ) => {
                let record_id = match typ {
                    Type::Custom(id) => id,
                    _ => {unreachable!()}
                };
                let record = interpreter.get_record(&record_id);
                if record.members.len() != args.len() {
                    return Err(State::Error(runtime_error(
                        format!("'{}' record has {} members but {} given", record.name, record.members.len(), args.len()), loc.to_owned()
                    )));
                }
                let mut values = HashMap::new();
                for i in 0..record.members.len() {
                    values.insert(record.members[i].clone(), args[i].clone());
                }
                let id = interpreter.ins_id;
                interpreter.instances.insert(id, values);
                interpreter.ins_id += 1;
                Ok(Value::Instance(*record_id, id))
            }
            _ => Err(State::Error(runtime_error(
                format!("Tried to apply to '{}', only 'Function's or 'Record's can be applied to", self.get_type().get_string(interpreter)), loc.to_owned()
            )))
        }
    }

    fn equality(self, rhs_value: Value, interpreter: &mut Interpreter, loc: Location) -> EvalResult {
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
                Err(State::Error(runtime_error(
                    format!("Can't check equality between '{}' and '{}'", self.get_type().get_string(interpreter), rhs_value.get_type().get_string(interpreter)), loc
                )))
            }
        }
    }
}

pub struct Interpreter {
    current_scope: Scope,
    globals      : Environment,
    
    repl         : bool,

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
        let module_val = self.eval(Node::Identifier(module_name.clone(), Location::new(0, 0, "erruer".into())))
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
        let module_val = self.eval(Node::Identifier(module_name.clone(), Location::new(0, 0, "erreur".into())))
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

    pub fn add_record(&mut self, global: bool, record: Record) {
        let id = self.record_id;
        self.records.insert(id, record.clone());
        self.record_id += 1;
        if global {
            self.globals.insert(
                record.name, 
                Value::Type(Type::Custom(id))
            );
        }
        else {
            self.current_scope.env.insert(
                record.name,
                Value::Type(Type::Custom(id))
            );
        }
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
            Node::Module(statements, _loc) => {
                for statement in statements {
                    match self.eval(statement) {
                        Ok(v) => if self.repl && v != Value::Nothing {
                            println!("{}", v.get_string(self));
                        },
                        Err(err) => return Err(err)
                    }
                } 
                Ok(Value::Nothing)
            },
            Node::Block(statements, _loc) => {
                self.enter_scope();
                for statement in statements {
                    if let Err(state) = self.eval(statement) {
                        self.exit_scope(); return Err(state)
                    }
                }
                self.exit_scope();
                Ok(Value::Nothing)
            },
            Node::ModuleDeclaration { name, body, loc: _ } => {
                self.enter_scope();
                let up = self.current_scope.upper.clone();
                self.current_scope.upper = Some(Box::new(Scope::new()));
                if let Err(state) = self.eval(*body) {
                    self.exit_scope(); return Err(state)
                }
                let scope = self.current_scope.clone();
                self.current_scope.upper = up;
                self.exit_scope();
                
                self.add_module(false, scope, name);
                Ok(Value::Nothing)
            }
            Node::Import(path, _loc) => {
                if path == "math" {
                    math_module::init(self);
                    return Ok(Value::Nothing);
                }
                
                let source = fs::read_to_string(path.clone())
                    .expect("\n  Error while reading the file\n");     
    

                let mut lexer = Lexer::new(path.clone());
                let tokens = match lexer.tokens(source) {
                    Ok(tokens) => tokens,
                    Err(error) => {
                        println!("{}", error);
                        exit(0);
                    }
                };
                let mut parser = Parser::new();
                let node = match parser.parse(tokens) {
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
                Ok(Value::Nothing)
            },
            Node::MapLiteral(map, _loc) => {
                let mut hmap = HashMap::new();
                for (k, v) in map {
                    let val = self.eval(k.clone())?;
                    let key = val.to_keyvalue(); 
                    match key {
                        Some(_) => {hmap.insert(key.unwrap(), self.eval(v)?);},
                        None => return Err(State::Error(runtime_error(
                            format!("Unhashable type '{}'", val.get_type().get_string(self)),
                            k.get_loc()
                        ))),
                    }
                }
                Ok(self.add_map(hmap))
            }
            Node::ForStatement { var, iter, body, loc: _ } => {
                let val = self.eval(*iter.clone())?;
                match val {
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
                                    _ => {
                                        self.exit_scope();
                                        return Err(s)
                                    }
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
                                    _ => {
                                        self.exit_scope();
                                        return Err(s)
                                    }
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
                                    _ => {
                                        self.exit_scope();
                                        return Err(s)
                                    }
                                }
                            };
                        }
                        self.exit_scope();
                    }
                    _ => return Err(State::Error(runtime_error(
                        format!("Can iterate only on 'Range', 'String' or 'List', not '{}'", val.get_type().get_string(self)),
                        iter.get_loc()
                    ))),
                }
                Ok(Value::Nothing)
            }
            Node::LetStatement { name, expr, loc } => {
                let val = self.eval(*expr)?;
                if !self.current_scope.env.contains_key(&name) {
                    self.current_scope.env.insert(name, val); Ok(Value::Nothing)
                } else { 
                    Err(State::Error(runtime_error(
                        format!("Variable '{}' already defined", name),
                        loc
                    )))
                }
            },
            Node::Return(expr, _loc) => Err(State::Return(self.eval(*expr)?)),
            Node::IfExpression { cond, tru, fals, loc: _ } => {
                let val = self.eval(*cond.clone())?;
                match val {
                    Value::Bool(t) => if t { Ok(self.eval(*tru)?) } 
                    else { Ok(self.eval(*fals)?) }
                    _ => Err(State::Error(runtime_error(
                        format!("Expected Bool at If Expression, got '{}'", val.get_type().get_string(self)),
                        cond.get_loc()
                    ))) 
                } 
            }
            Node::RecordDeclaration { name, members, loc: _ } => {
                self.add_record(false, Record {name, members});
                Ok(Value::Nothing)                
            },
            Node::FunctionDeclaration { name, args, body, loc: _ } => {
                let closure = if let Some(_) = self.current_scope.upper {
                    Some(self.current_scope.env.clone())
                } else { None };
                if !name.is_empty() {
                    self.add_function(Function { name, args, body: *body, closure });
                    Ok(Value::Nothing)
                } else {
                    let fname ="lambda".into();
                    self.add_function(Function { name: fname, args, body: *body, closure });
                    Ok(Value::Function(self.func_id - 1))
                }
            },
            Node::IfStatement { expr, body, els, loc: _ } => {
                let val = self.eval(*expr.clone())?;
                match val {
                    Value::Bool(t) => if t { Ok(self.eval(*body)?) } 
                    else { Ok(self.eval(*els)?) }
                    _ => Err(State::Error(runtime_error(
                        format!("Expected Bool at If Statement, got '{}'", val.get_type().get_string(self)),
                        expr.get_loc()
                    ))) 
                } 
            }
            Node::WhileStatement { expr, body, loc: _ } => loop {
                let val = self.eval(*expr.clone())?;
                match  val {
                    Value::Bool(t) => if t {
                        match self.eval(*body.clone()) {
                            Ok(_) => {},
                            Err(s) => match s {
                                State::Continue => continue,
                                State::Break => break Ok(Value::Nothing),
                                _ => return Err(s)
                            }
                        };
                    } else { return Ok(Value::Nothing) }
                    _ => return Err(State::Error(runtime_error(
                        format!("Expected Bool at While Statement, got '{}'", val.get_type().get_string(self)),
                        expr.get_loc()
                    ))) 
                }
            },
            Node::BinaryExpression { op, lhs, rhs, loc } => {
                if op.as_str() == "=" {
                    let right_value = self.eval(*rhs)?;
                    return match *lhs {
                        Node::Identifier(var, loc) => 
                            match self.current_scope.assign(&var, &right_value, loc) {
                                Ok(()) => Ok(right_value),
                                Err(err) => Err(State::Error(err))
                            },
                        Node::BinaryExpression { op, lhs, rhs, loc } => {
                            let llhs_value = self.eval(*lhs.clone())?;
                            if op.as_str() == "." {
                                return match (llhs_value.clone(), *rhs) {
                                    (Value::Instance(_, id), Node::Identifier(member, _loc)) => {
                                        let ins = self.get_instance_mut(&id);
                                        let attr = ins.get_mut(&member).unwrap();
                                        *attr = right_value.clone();
                                        Ok(right_value)
                                    }
                                    (Value::Module(id), Node::Identifier(member, loc)) => {
                                        let module = self.get_module_mut(&id);
                                        match module.scope.assign(&member, &right_value, loc) {
                                            Ok(()) => Ok(right_value),
                                            Err(err) => Err(State::Error(err))
                                        }
                                    },
                                    _ => Err(State::Error(runtime_error(
                                        format!("Only can access members of 'Instance' or 'Module', not '{}'", llhs_value.get_type().get_string(self)),
                                        loc
                                    ))) 
                                }                            }
                            let lrhs_value = self.eval(*rhs.clone())?;
                            if op.as_str() == "[" {
                                return match (llhs_value.clone(), lrhs_value.clone()) {
                                    (Value::List(id), Value::Int(index)) => {
                                        let list = self.get_list_mut(&id);
                                        if index as usize >= list.len() {
                                            return Err(State::Error(runtime_error(
                                                format!("Got index '{}' but lenght of list is '{}'", index, list.len()),
                                                rhs.get_loc()
                                            ))) 
                                        }
                                        let _ = std::mem::replace(&mut list[index as usize], right_value.clone());
                                        Ok(right_value)
                                    },
                                    (Value::Map(id), _) => {
                                        let key = lrhs_value.to_keyvalue();
                                        if key.is_none() {
                                            return Err(State::Error(runtime_error(
                                                format!("Unhashable type '{}'", lrhs_value.get_type().get_string(self)),
                                                lhs.get_loc()
                                            )))
                                        }
                                        let map = self.get_map_mut(&id);
                                        map.insert(key.unwrap(), right_value.clone());
                                        Ok(right_value)
                                    }
                                    _ => Err(State::Error(runtime_error(
                                        format!("Only assign to members of 'Map' and 'List', not '{}'", llhs_value.get_type().get_string(self)),
                                        loc
                                    )))
                                }
                            }
                            Err(State::Error(runtime_error(
                                format!("Unvalid assignment target"),
                                lhs.get_loc()
                            )))
                        }
                        _ => Err(State::Error(runtime_error(
                            format!("Unvalid assignment target"),
                            lhs.get_loc()
                        )))
                    }
                }
                if op.as_str() == "." {
                    let left_value = self.eval(*lhs.clone())?;
                    return match (left_value.clone(), *rhs.clone()) {                            
                        (Value::Instance(rid, id), Node::Identifier(member, loc)) => {
                            let ins = self.get_instance(&id);
                            match ins.get(&member) {
                                Some(val) => Ok(val.clone()),
                                None => Err(State::Error(runtime_error(
                                    format!("'{}' Record has no member called '{}'", self.get_record(&rid).name, member),
                                    loc
                                )))
                            }
                        }
                        (Value::Module(id), Node::Identifier(member, loc)) => {
                            let module = self.get_module(&id);
                            let res = module.scope.resolve(&member);
                            if res.is_none() {
                                Err(State::Error(runtime_error(
                                    format!("'{}' Module has no member called '{}'", module.name, member),
                                    loc
                                )))                            
                            } else { Ok(res.unwrap()) }
                        },
                        _ => Err(State::Error(runtime_error(
                            format!("Only can access members of 'Instance' or 'Module', not '{}'", left_value.get_type().get_string(self)),
                            lhs.get_loc()
                        )))
                    }
                }
                let left_value = self.eval(*lhs.clone())?;
                let right_value = self.eval(*rhs.clone())?;
                match op.as_str() {
                    "+" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 + b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't add '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), lhs.get_loc()
                        )))
                    } 
                    "-" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 - b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't subtract '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    }
                    "*" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f32 * b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't multiply '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    }
                    "/" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) =>
                            if b != 0 { Ok(Value::Int(a / b)) } 
                            else { Err(State::Error(runtime_error("Attempt to divide by 0".to_string(), rhs.get_loc()))) }
                        (Value::Float(a), Value::Float(b)) =>
                            if b != 0.0 { Ok(Value::Float(a / b)) } 
                            else { Err(State::Error(runtime_error("Attempt to divide by 0".to_string(), rhs.get_loc()))) }
                        (Value::Float(a), Value::Int(b)) => 
                            if b != 0 { Ok(Value::Float(a / b as f32)) } 
                            else { Err(State::Error(runtime_error("Attempt to divide by 0".to_string(), rhs.get_loc()))) }
                        (Value::Int(a), Value::Float(b)) => 
                            if b != 0.0 { Ok(Value::Float(a as f32 / b)) } 
                            else { Err(State::Error(runtime_error("Attempt to divide by 0".to_string(), rhs.get_loc()))) }
                        _ => Err(State::Error(runtime_error(
                            format!("Can't divide '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    }
                    "%" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't modulo '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    }
                    "<=" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a <= b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 <= b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't compare '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    },
                    ">=" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a >= b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 >= b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't compare '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    },
                    "<" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a < b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool((a as f32) < b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't compare '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    },
                    ">" => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(a > b as f32)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(a as f32 > b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't compare '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    },
                    "!=" => if let Value::Bool(b) = left_value.equality(right_value, self, loc)? {
                            return Ok(Value::Bool(!b));
                        } else { unreachable!() }
                    "==" => left_value.equality(right_value, self, loc),
                    "||" => match (left_value.clone(), right_value.clone()) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't logic-or '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))
                    },
                    "&&" => match (left_value.clone(), right_value.clone()) {
                        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                        _ => Err(State::Error(runtime_error(
                            format!("Can't logic-and '{}' and '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                        )))                    },
                    ".." => match (left_value.clone(), right_value.clone()) {
                        (Value::Int(a), Value::Int(b)) => 
                            if a <= b {
                                Ok(Value::Range(a, b))
                            } else { 
                                Err(State::Error(runtime_error(
                                format!("Upper-bound '{}' is less than lower-bound '{}'", left_value.get_type().get_string(self), right_value.get_type().get_string(self)), loc
                                ))) 
                            }
                        _ => Err(State::Error(runtime_error(
                            format!("Can only create range out of 'Integer's, not '{}'", left_value.get_type().get_string(self)), loc
                        )))
                    },
                    "::" => match (left_value.clone(), right_value.clone()) {
                        (_, Value::Type(typ)) => Ok(Value::Bool(left_value.get_type() == typ)),
                        _ => Err(State::Error(runtime_error(
                            format!("Right-side is expected to be a 'Type' but got, '{}'", right_value.get_type().get_string(self)),
                            rhs.get_loc()
                        )))
                    },
                    "[" => match (left_value.clone(), right_value.clone()) {
                        (Value::List(id), Value::Int(index)) => {
                            let list = self.get_list(&id);
                            if index as usize >= list.len() {
                                return Err(State::Error(runtime_error(
                                    format!("Got index '{}' but lenght of list is '{}'", index, list.len()),
                                    rhs.get_loc()
                                )))                            }
                            Ok(self.get_list(&id)[index as usize].clone())
                        },
                        (Value::String(string), Value::Int(index)) => {
                            let res = string.chars().nth(index as usize);
                            if res.is_none() { 
                                Err(State::Error(runtime_error(
                                    format!("Got index '{}' but lenght of String is '{}'", index, string.len()),
                                    rhs.get_loc()
                                )))
                            }
                            else { Ok(Value::String(res.unwrap().to_string())) }
                        },
                        (Value::Map(id), _) => {
                            let key = right_value.to_keyvalue();
                            if key.is_none() {
                                return Err(State::Error(runtime_error(
                                    format!("Unhashable type '{}'", right_value.get_type().get_string(self)),
                                    rhs.get_loc()
                                )))
                            }
                            let map = self.get_map(&id);
                            let o = &key.unwrap();
                            if !map.contains_key(o) {
                                Err(State::Error(runtime_error(
                                    format!("Map has no field called '{}'", right_value.get_string(self)),
                                    rhs.get_loc()
                                )))
                            } else {
                                Ok(map[o].clone())
                            }
                        }
                        _ => Err(State::Error(runtime_error(
                            format!("Can only index into 'List', 'String', and 'Map' not '{}'", left_value.get_type().get_string(self)), loc
                        )))
                    }
                    "|>" => right_value.apply(self, &[left_value], &loc), 
                    _ => unreachable!() 
                }
            },
            Node::IntegerLiteral(i, _loc) => Ok(Value::Int(i)),
            Node::FloatLiteral(f, _loc)   => Ok(Value::Float(f)),
            Node::StringLiteral(s, _loc)  => Ok(Value::String(s)),
            Node::True(_loc)              => Ok(Value::Bool(true)),
            Node::False(_loc)             => Ok(Value::Bool(false)),
            Node::Nothing(_loc)           => Ok(Value::Nothing),
            Node::Break(_loc)             => Err(State::Break),
            Node::Continue(_loc)          => Err(State::Continue),
            Node::Group(expr, _loc)       => self.eval(*expr),
            Node::ListLiteral(nodes, _loc) => {
                let values = self.collect_values(nodes)?;
                Ok(self.add_list(values))
            },
            Node::Identifier(s, loc) => match self.current_scope.resolve(&s) {
                Some(value) => Ok(value),
                None => match self.globals.get(&s) {
                    Some(val) => Ok(val.clone()),
                    None => Err(State::Error(runtime_error(
                        format!("Undefined Variable '{}'", s), loc
                    )))
                }
            },
            Node::UnaryExpression { op, operand, loc } => {
                let operand_value = self.eval(*operand)?;
                match op.as_str() {
                    "+" => match operand_value {
                        Value::Int(a)   => Ok(Value::Int(a)),
                        Value::Float(a) => Ok(Value::Float(a)),
                        _ => Err(State::Error(runtime_error(
                            format!("Only unary-plus 'Integer' or 'Float', not '{}'", operand_value.get_type().get_string(self)), loc
                        )))                    }
                    "-" => match operand_value {
                        Value::Int(a)   => Ok(Value::Int(-a)),
                        Value::Float(a) => Ok(Value::Float(-a)),
                        _ => Err(State::Error(runtime_error(
                            format!("Only negate 'Integer' or 'Float', not '{}'", operand_value.get_type().get_string(self)), loc
                        )))                    }
                    "!" => match operand_value {
                        Value::Bool(a) => Ok(Value::Bool(!a)),
                        _ => Err(State::Error(runtime_error(
                            format!("Only logic-not 'Bool', not '{}'", operand_value.get_type().get_string(self)), loc
                        )))
                    }
                    _ => unreachable!()
                }
            },
            Node::Application { fun, args, loc } => {
                let values = self.collect_values(args)?;
                self.eval(*fun)?.apply(self, &values, &loc)
            },
        }
    }
    
}