use crate::interpreter::{ Interpreter, NativeFunction, RuntimeError, Record, State };
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
    
    interpreter.add_global_variable("Nothing".to_string(),
        Value::Type(Type::Unit)
    );

    interpreter.add_global_variable("Range".to_string(),
        Value::Type(Type::Range)
    );

    interpreter.add_record(Record { 
        name: "Error".into(), members: vec!["value".into()] 
    });

    interpreter.add_native_function(NativeFunction::new( 
        String::from("ensure"), 
        1,
        |interpreter, args| {
            match args[0].get_type() {
                Type::Custom(id) => if "Error".to_string() == interpreter.get_record(&id).name {
                    return Err(State::Error(RuntimeError::ReturnedError));
                }
                _ => {}
            }
            Ok(args[0].clone())
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("print"), 
        1,
        |interpreter, args| {
            println!("{}", args[0].get_string(interpreter));
            Ok(Value::None)
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
                Value::String(string) => Ok(Value::Int(string.len() as i32)),
                _ => Err(State::Error(RuntimeError::TypeMismatch))
            }
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("map"), 
        2, 
        |interpreter, args| {
            match (&args[0], &args[1]) {
                (_, Value::List(list_id)) => {
                    let list = interpreter.get_list(list_id).clone();

                    let res = list.iter().map(|element| {
                        args[0].apply(interpreter, &[element.clone()])
                            .expect("Value Expected")
                    }).collect::<Vec<_>>();

                    *interpreter.get_list_mut(list_id) = res;
                    Ok(Value::None)
                },
                _ => Err(State::Error(RuntimeError::TypeMismatch))
            }
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("assert"), 
        1,
        |_, args| {
            match args[0] {
                Value::Bool(t) =>
                    if !t { Err(State::Error(RuntimeError::AssertionFailure)) }
                    else { Ok(Value::None) },
                _ => Err(State::Error(RuntimeError::TypeMismatch))
            } 
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("append"), 
        2,
        |interpreter: &mut Interpreter, args| {
            match args[0] {
                Value::List(idx) => {
                    interpreter.get_list_mut(&idx).push(args[1].clone());
                    Ok(Value::None)
                }
                _ => Err(State::Error(RuntimeError::TypeMismatch))
            } 
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("pop"), 
        1,
        |interpreter: &mut Interpreter, args| {
            match args[0] {
                Value::List(idx) => {
                    interpreter.get_list_mut(&idx).pop();
                    Ok(Value::None)
                }
                _ => Err(State::Error(RuntimeError::TypeMismatch))
            } 
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("clear"), 
        1,
        |interpreter: &mut Interpreter, args| {
            match args[0].clone() {
                Value::List(idx) => {
                    interpreter.get_list_mut(&idx).clear();
                    Ok(Value::None)
                },
                Value::String(mut string) => {
                    string.clear();
                    Ok(Value::None)
                }
                _ => Err(State::Error(RuntimeError::TypeMismatch))
            } 
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("contains"), 
        2,
        |interpreter: &mut Interpreter, args| {
            match args[0] {
                Value::List(idx) => {
                    Ok(Value::Bool(interpreter.get_list(&idx).contains(&args[1])))
                },
                Value::Map(idx) => {
                    let key = match args[1].to_keyvalue() {
                        Some(_) => args[1].to_keyvalue().unwrap(),
                        None => {return Err(State::Error(RuntimeError::KeyError));}
                    };
                    Ok(Value::Bool(interpreter.get_map(&idx).contains_key(&key)))
                }
                _ => Err(State::Error(RuntimeError::TypeMismatch))
            } 
        } 
    ));
}