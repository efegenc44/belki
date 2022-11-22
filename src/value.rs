use crate::interpreter::{Interpreter};


#[derive(PartialEq)]
#[allow(dead_code)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    List,
    Custom(u64),
    Function,
    Module,
    ClassDef(u64),
    Method(u64, String),

    Unit,
    Void
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
    Module(u64),
    ClassD(u64),
    Method(u64 /* ins id */, u64 /* class id */, String /* method name */),
    
    Nothing, // Unit
    None,    // Void
}

impl Value {
    #[allow(dead_code)]
    fn get_type(&self) -> Type {
        match self {
            Value::Int(_)                   => Type::Int,
            Value::Float(_)                 => Type::Float,
            Value::Bool(_)                  => Type::Bool,
            Value::String(_)                => Type::String,
            Value::List(_)                  => Type::List,
            Value::Instance(cid, _)   => Type::Custom(*cid),
            Value::Function(_)              => Type::Function,
            Value::NativeFunction(_)        => Type::Function,
            Value::Module(_)                => Type::Module,
            Value::ClassD(cid)        => Type::ClassDef(*cid),
            Value::Method(_, 
                cid, name)   => Type::Method(*cid, name.clone()),
            Value::Nothing                  => Type::Unit,
            Value::None                     => Type::Void
        }
    }
    
    pub fn print(&self, interpreter: &mut Interpreter) {
        match self {
            Value::Int(i) => print!("{}", i),
            Value::Float(f) => print!("{}", f),
            Value::String(s) => print!("{}", s),
            Value::Bool(b) => print!("{}", b),
            Value::Nothing => print!("nothing"),
            Value::None => print!("none"),
            Value::List(id) => {
                print!("[");
                let mut first = true;
                for i in interpreter.get_list(id).clone() {
                    if !first { print!(", "); } else { first = false }  
                    i.print(interpreter);
                }
                print!("]")
            },
            Value::Function(id) => print!("function: {}", interpreter.get_function(id).name),
            Value::NativeFunction(id) => print!("native: {}", interpreter.get_nfunction(id).name),
            Value::ClassD(id) => print!("class: {}", interpreter.get_classdef(id).name),
            Value::Module(id) => print!("module: {}", interpreter.get_module(id).name),
            Value::Instance(cid, _) => {
                print!("instance of class {}", interpreter.get_classdef(cid).name)
            }            
            Value::Method(_, cid, name) => {
                print!("method '{}' of {}", name, interpreter.get_classdef(cid).name);
            }
        }
    }
}