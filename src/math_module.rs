use crate::interpreter::{ Interpreter, Scope, NativeFunction };
use crate::value::Value;

pub fn init(interpreter: &mut Interpreter) {
    interpreter.add_module(true, Scope::new(),"math".to_string());
    
    interpreter.add_native_function_to_module("math".to_string(), NativeFunction::new( 
        String::from("pow"), 
        2, 
        |_, args| {
            match (args[0].clone(), args[1].clone()) {
                (Value::Int(base), Value::Int(power)) => {
                    return Value::Int(base.pow(power as u32));
                },
                _ => {Value::None}
            }
        } 
    ));
    
    interpreter.add_native_function_to_module("math".to_string(), NativeFunction::new( 
        String::from("sqrt"), 
        1, 
        |_, args| {
            match args[0].clone() {
                Value::Int(i) => {
                    return Value::Float(f32::sqrt(i as f32));
                },
                _ => {Value::None}
            }
        } 
    ));
}