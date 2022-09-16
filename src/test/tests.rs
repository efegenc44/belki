#[cfg(test)]
mod tests {
    #[test]
    fn factorial() {
        let res = crate::from_file(String::from("src/test/fact.txt"));
        assert!(res != None);
    }
    
    #[test]
    fn fibonacci() {
        let res = crate::from_file(String::from("src/test/fib.txt"));
        assert!(res != None);
    }
    
    #[test]
    fn math_lib() {
        let res = crate::from_file(String::from("src/test/math_lib.txt"));
        assert!(res != None);
    }

    #[test]
    fn some_test() {
        let res = crate::from_file(String::from("src/test/some_test.txt"));
        assert!(res != None);
    }

    #[test]
    fn closure() {
        let res = crate::from_file(String::from("src/test/closure.txt"));
        assert!(res != None);
    }
    
    #[test]
    fn import_class_mix() {
        let res = crate::from_file(String::from("src/test/import_class_mix.txt"));
        assert!(res != None);
    }

    #[test]
    fn while_loop() {
        let res = crate::from_file(String::from("src/test/while_loop.txt"));
        assert!(res != None);
    }
}