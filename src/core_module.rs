use crate::interpreter::{ Interpreter, NativeFunction };
use crate::value::Value;

pub fn init(interpreter: &mut Interpreter) {
    
    interpreter.add_native_function(NativeFunction::new( 
        String::from("print"), 
        1,
        |_, args| {
            println!("{:?}", args[0]); Value::None
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("len"), 
        1, 
        |interpreter, args| {
            match &args[0] {
                Value::List(id) => {
                    let list = interpreter.get_list(id);
                    Value::Int(list.len() as i32)
                },
                Value::String(string) => {
                    Value::Int(string.len() as i32)
                }
                _ => Value::None
            }
        } 
    ));
}