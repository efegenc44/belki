use crate::interpreter::{ Interpreter, NativeFunction, RuntimeError };
use crate::value::{ Value, Type };

pub fn init(interpreter: &mut Interpreter) {
    
    interpreter.add_global_variable("Integer".to_string(),
        Value::Type(Type::Int)
    );

    interpreter.add_global_variable("Float".to_string(),
        Value::Type(Type::Float)
    );

    interpreter.add_global_variable("List".to_string(),
        Value::Type(Type::List)
    );

    interpreter.add_global_variable("Bool".to_string(),
        Value::Type(Type::Bool)
    );

    interpreter.add_global_variable("String".to_string(),
        Value::Type(Type::String)
    );

    interpreter.add_global_variable("Function".to_string(),
        Value::Type(Type::Function)
    );

    interpreter.add_global_variable("Module".to_string(),
        Value::Type(Type::Module)
    );
    
    interpreter.add_global_variable("Method".to_string(),
        Value::Type(Type::Method)
    );

    interpreter.add_global_variable("Nothing".to_string(),
        Value::Type(Type::Unit)
    );

    interpreter.add_native_function(NativeFunction::new( 
        String::from("print"), 
        1,
        |interpreter, args| {
            for arg in args {
                println!("{}", arg.get_string(interpreter));
            }; Ok(Value::None)
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("type"), 
        1,
        |_, args| {
            Ok(Value::Type(args[0].get_type()))
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

    interpreter.add_native_function(NativeFunction::new( 
        String::from("assert"), 
        1,
        |_, args| {
            match args[0] {
                Value::Bool(t) =>
                    if !t { Err(RuntimeError::AssertionFailure) }
                    else { Ok(Value::None) },
                _ => Err(RuntimeError::TypeMismatch)
            } 
        } 
    ));
}