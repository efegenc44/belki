use crate::interpreter::Interpreter;


#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Type,

    Int,
    Float,
    Bool,
    String,
    List,
    Custom(u64),
    Function,
    Range,
    Module,

    Unit,
    Void
}

impl Type {
    pub fn get_string(self, interpreter: &mut Interpreter) -> String {
        match self {
            Type::Type => "Type".to_string(),
            
            Type::Int => "Integer".to_string(),
            Type::Float => "Float".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::String => "String".to_string(),
            Type::List => "List".to_string(),
            Type::Function => "Function".to_string(),
            Type::Module => "Module".to_string(),
            Type::Range => "Range".to_string(),
            Type::Custom(cid) => interpreter.get_classdef(&cid).name.clone(),
            
            Type::Unit => "Unit".to_string(),
            Type::Void => "Void".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    List(u64),
    Instance(u64 /* class id */, u64 /* ins id */),
    Function(u64),
    NativeFunction(u64),
    Range(i32, i32),
    Module(u64),
    Type(Type),

    Nothing, // Unit
    None,    // Void
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Int(_)            => Type::Int,
            Value::Float(_)          => Type::Float,
            Value::Bool(_)           => Type::Bool,
            Value::String(_)         => Type::String,
            Value::List(_)           => Type::List,
            Value::Instance(cid, _)  => Type::Custom(*cid),
            Value::Function(_)       => Type::Function,
            Value::NativeFunction(_) => Type::Function,
            Value::Range(_, _)       => Type::Range,
            Value::Module(_)         => Type::Module,
            Value::Type(_)           => Type::Type,
            
            Value::Nothing           => Type::Unit,
            Value::None              => Type::Void // ?
        }
    }
    
    pub fn get_string(&self, interpreter: &mut Interpreter) -> String {
        match self {
            Value::Type(typ) => format!("<type: {}>", typ.clone().get_string(interpreter)),
            
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => format!("'{}'", s.clone()),
            Value::Bool(b) => if *b { "true".to_string() } else { "false".to_string() },
            Value::Nothing => "nothing".to_string(),
            Value::None => "none".to_string(), // ?
            Value::List(id) => {
                let mut s = String::from("[");
                let mut first = true;
                for i in interpreter.get_list(id).clone() {
                    if !first { s += ", "; } else { first = false }  
                    s += &i.get_string(interpreter);
                } s += "]"; s
            },
            Value::Function(id) => format!("<function: {}>", interpreter.get_function(id).name),
            Value::NativeFunction(id) => format!("<native: {}>", interpreter.get_nfunction(id).name),
            Value::Range(_, _) => format!("<range>"),
            Value::Module(id) => format!("<module: {}>", interpreter.get_module(id).name),
            Value::Instance(_, id) => {
                let mut s = String::from("{");
                let mut first = true;
                for (key ,value) in interpreter.get_instance(id).clone() {
                    if !first { s += ", " } else { first = false; }
                    s += &format!("{}: {}", key, value.get_string(interpreter));
                } s += "}"; s
            }            
        }
    }
}