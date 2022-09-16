use crate::interpreter::{ Interpreter, NativeFunction, RuntimeError };
use crate::value::Value;

pub fn init(interpreter: &mut Interpreter) {
    
    interpreter.add_native_function(NativeFunction::new( 
        String::from("print"), 
        1,
        |_, args| {
            println!("{:?}", args[0]); Ok(Value::None)
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("len"), 
        1, 
        |interpreter, args| {
            match &args[0] {
                Value::List(id) => {
                    let list = interpreter.get_list(id);
                    Ok(Value::Int(list.len() as i32))
                },
                Value::String(string) => {
                    Ok(Value::Int(string.len() as i32))
                }
                _ => Err(RuntimeError::TypeMismatch)
            }
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("map_native"), 
        2, 
        |interpreter, args| {
            match (&args[0], &args[1]) {
                (Value::Function(func_id), Value::List(list_id)) => {
                    let function = interpreter.get_function(func_id).clone();
                    let list = interpreter.get_list(list_id).clone();

                    let res = list.iter().map(|element| {
                        function.call(*func_id, interpreter, &[element.clone()])
                            .expect("Value Expected")
                    }).collect::<Vec<_>>();

                    *interpreter.get_list_mut(list_id) = res;
                    Ok(Value::None)
                },
                (Value::NativeFunction(func_id), Value::List(list_id)) => {
                    let function = interpreter.get_nfunction(func_id).clone();
                    let list = interpreter.get_list(list_id).clone();

                    let res = list.iter().map(|element| {
                        function.call(interpreter, &[element.clone()])
                            .expect("Value Expected")
                    }).collect::<Vec<_>>();

                    *interpreter.get_list_mut(list_id) = res;
                    Ok(Value::None)
                }
                _ => Err(RuntimeError::TypeMismatch)
            }
        } 
    ));
}