use crate::interpreter::{ Interpreter, NativeFunction, Record, State, runtime_error };
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

    interpreter.add_record(true, Record { 
        name: "Error".into(), members: vec!["value".into()] 
    });

    interpreter.add_native_function(NativeFunction::new( 
        String::from("ensure"), 
        1,
        |interpreter, args, loc| {
            match args[0].get_type() {
                Type::Custom(id) => if "Error".to_string() == interpreter.get_record(&id).name {
                    return Err(State::Error(runtime_error(
                        format!("Expression returned Error value"), loc.to_owned()
                    )))
                }
                _ => {}
            }
            Ok(args[0].clone())
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("print"), 
        1,
        |interpreter, args, _| {
            println!("{}", args[0].get_string(interpreter));
            Ok(Value::Nothing)
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("string"), 
        1,
        |interpreter, args, _| {
            Ok(Value::String(args[0].get_string(interpreter)))
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("list"), 
        1,
        |interpreter, args, loc| {
            match args[0] {
                Value::Range(a, b) => {
                    let list: Vec<_> = (a..b).into_iter().map(|x| Value::Int(x)).collect();
                    Ok(interpreter.add_list(list))
                },
                _ => Err(State::Error(runtime_error(
                    format!("Can't convert '{}' to List", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))
            }
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("type"), 
        1,
        |_, args, _| {
            Ok(Value::Type(args[0].get_type()))
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("len"), 
        1, 
        |interpreter, args, loc| {
            match &args[0] {
                Value::List(id) => {
                    let list = interpreter.get_list(id);
                    Ok(Value::Int(list.len() as i32))
                },
                Value::String(string) => Ok(Value::Int(string.len() as i32)),
                _ => Err(State::Error(runtime_error(
                    format!("Can only get lenght of 'List' to 'String', not '{}'", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))
            }
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("map_mut"), 
        2, 
        |interpreter, args, loc| {
            match (&args[0], &args[1]) {
                (_, Value::List(list_id)) => {
                    let list = interpreter.get_list(list_id).clone();

                    let mut res = vec![];
                    for element in list {
                        res.push(args[0].apply(interpreter, &[element], loc)?)
                    }

                    *interpreter.get_list_mut(list_id) = res;
                    Ok(Value::Nothing)
                },
                _ => Err(State::Error(runtime_error(
                    format!("Can only map_mut over 'List', not '{}'", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))
            }
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("map"), 
        2, 
        |interpreter, args, loc| {
            match (&args[0], &args[1]) {
                (_, Value::List(list_id)) => {
                    let list = interpreter.get_list(list_id).clone();

                    let mut res = vec![];
                    for element in list {
                        res.push(args[0].apply(interpreter, &[element], loc)?)
                    }

                    let ls = interpreter.add_list(res);
                    Ok(ls)
                },
                _ => Err(State::Error(runtime_error(
                    format!("Can only map over 'List', not '{}'", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))
            }
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("assert"), 
        1,
        |interpreter, args, loc| {
            match args[0] {
                Value::Bool(t) =>
                    if !t { 
                        Err(State::Error(runtime_error(format!("Assertion failed"), loc.to_owned()))) 
                    }
                    else { Ok(Value::Nothing) },
                _ => Err(State::Error(runtime_error(
                    format!("Expected Boolin assert but got '{}'", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))
            } 
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("append"), 
        2,
        |interpreter: &mut Interpreter, args, loc| {
            match args[0] {
                Value::List(idx) => {
                    interpreter.get_list_mut(&idx).push(args[1].clone());
                    Ok(Value::Nothing)
                }
                _ => Err(State::Error(runtime_error(
                    format!("Can only append to 'List', not '{}'", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))
            } 
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("pop"), 
        1,
        |interpreter: &mut Interpreter, args, loc| {
            match args[0] {
                Value::List(idx) => {
                    match interpreter.get_list_mut(&idx).pop() {
                        Some(val) => Ok(val),
                        None => Err(State::Error(runtime_error(
                            format!("List is empty can't pop"), loc.to_owned()
                        ))) 
                    }
                }
                _ => Err(State::Error(runtime_error(
                    format!("Can only pop from 'List', not '{}'", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))            
            } 
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("clear"), 
        1,
        |interpreter: &mut Interpreter, args, loc| {
            match args[0].clone() {
                Value::List(idx) => {
                    interpreter.get_list_mut(&idx).clear();
                    Ok(Value::Nothing)
                },
                Value::String(mut string) => {
                    string.clear();
                    Ok(Value::Nothing)
                }
                _ => Err(State::Error(runtime_error(
                    format!("Can only clear 'List' and 'String', not '{}'", args[0].get_type().get_string(interpreter)),
                    loc.to_owned()
                )))
            } 
        } 
    ));

    interpreter.add_native_function(NativeFunction::new( 
        String::from("contains"), 
        2,
        |interpreter: &mut Interpreter, args, loc| {
            match args[0] {
                Value::List(idx) => {
                    Ok(Value::Bool(interpreter.get_list(&idx).contains(&args[1])))
                },
                Value::Map(idx) => {
                    let key = match args[1].to_keyvalue() {
                        Some(_) => args[1].to_keyvalue().unwrap(),
                        None => {
                            return Err(State::Error(runtime_error(
                                format!("Unhashable type '{}'", args[1].get_type().get_string(interpreter)), loc.to_owned()
                            )))
                        }
                    };
                    Ok(Value::Bool(interpreter.get_map(&idx).contains_key(&key)))
                }
                _ => Err(State::Error(runtime_error(
                    format!("Can only 'contains' 'List' and 'Map', not '{}'", args[0].get_type().get_string(interpreter)), loc.to_owned()
                )))
            } 
        } 
    ));
}