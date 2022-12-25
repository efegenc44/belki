use crate::interpreter::{ Interpreter, Scope, NativeFunction, State, runtime_error };
use crate::value::Value;

pub fn init(interpreter: &mut Interpreter) {
    interpreter.add_module(true, Scope::new(),"math".to_string());
    
    interpreter.add_native_function_to_module("math".to_string(), NativeFunction::new( 
        String::from("pow"), 
        2, 
        |interpreter, args, loc| {
            match (args[0].clone(), args[1].clone()) {
                (Value::Int(base), Value::Int(power)) => {
                    return Ok(Value::Int(base.pow(power as u32)));
                },
                _ => Err(State::Error(runtime_error(
                    format!("Can only take power of 'Integer' to 'Integer', not '{}' to '{}'", args[0].get_type().get_string(interpreter), args[1].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))
            }
        } 
    ));
    
    interpreter.add_native_function_to_module("math".to_string(), NativeFunction::new( 
        String::from("sqrt"), 
        1, 
        |interpreter, args, loc| {
            match args[0].clone() {
                Value::Int(i) => {
                    return Ok(Value::Float(f32::sqrt(i as f32)));
                },
                Value::Float(f) => {
                    return Ok(Value::Float(f32::sqrt(f)));
                },
                _ => Err(State::Error(runtime_error(
                    format!("Can only take Square-root of 'Integer' or 'Float', not '{}'", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))            
            }
        } 
    ));

    interpreter.add_variable_to_module("math".to_string(), "pi".to_string(), Value::Float(std::f32::consts::PI))
}