use std::collections::HashMap;

pub enum RuntimeError {
    TypeMismatch,
    ArgNumMismatch,
    AssignmentError,
    MemberAccessError,
    IndexOutOfRange,
    UndefinedVariable,
    AlreadyDefinedVarible,
    DivisionByZero,
    AssertionFailure,
    BadRange,
    KeyError,
    HashError,
    ReturnedError,
}


pub struct Scope {
    env  : HashMap<String, Value>,
    upper: Option<Box<Scope>>
}

impl Scope {
    pub fn new() -> Scope {
        Scope { env: HashMap::new(), upper: None }
    }

    fn assign(&mut self, var: &String, val: &Value) -> Result<(), RuntimeError> {
        match self.env.get_mut(var) {
            Some(value) => {
                *value = val.clone(); Ok(())
            }
            None => match &mut self.upper {
                Some(scope) => scope.assign(var, val),
                None => Err(RuntimeError::UndefinedVariable)
            }
        }
    }

    fn resolve(&self, var: &String) -> Option<Value> {
        match self.env.get(var) {
            Some(value) => Some(value.clone()), 
            None => match &self.upper {
                Some(upper) => upper.resolve(var),
                None => None
            }
        }
    }
}