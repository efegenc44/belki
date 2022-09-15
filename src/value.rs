#[allow(dead_code)]
enum Type {
    Int,
    Float,
    Bool,
    String,
    List,
    Custom(String),
    Function,

    Unit
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