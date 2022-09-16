
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
}