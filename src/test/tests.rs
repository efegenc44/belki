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

    #[test]
    fn fizzbuzz_step() {
        let res = crate::from_file(String::from("src/test/fizzbuzz_step.txt"));
        assert!(res != None);
    }

    #[test]
    fn return_while() {
        let res = crate::from_file(String::from("src/test/return_while.txt"));
        assert!(res != None);
    }

    #[test]
    fn return_if() {
        let res = crate::from_file(String::from("src/test/return_if.txt"));
        assert!(res != None);
    }

    #[test]
    fn return_block() {
        let res = crate::from_file(String::from("src/test/return_block.txt"));
        assert!(res != None);
    }

    #[test]
    fn newline_awareness() {
        let res = crate::from_file(String::from("src/test/newline_awareness.txt"));
        assert!(res != None);
    }

    #[test]
    fn for_test() {
        let res = crate::from_file(String::from("src/test/for_test.txt"));
        assert!(res != None);
    }

    #[test]
    fn while_break() {
        let res = crate::from_file(String::from("src/test/while_break.txt"));
        assert!(res != None);
    }

    #[test]
    fn module_test() {
        let res = crate::from_file(String::from("src/test/module_test.txt"));
        assert!(res != None);
    }
    
    #[test]
    fn map() {
        let res = crate::from_file(String::from("src/test/map.txt"));
        assert!(res != None);
    }

    #[test]
    fn error() {
        let res = crate::from_file(String::from("src/test/error.txt"));
        assert!(res != None);
    }

    #[test]
    fn ifexpr() {
        let res = crate::from_file(String::from("src/test/ifexpr.txt"));
        assert!(res != None);
    }

    #[test]
    fn chained_relation() {
        let res = crate::from_file(String::from("src/test/chained_relation.txt"));
        assert!(res != None);
    }

    #[test]
    fn return_for() {
        let res = crate::from_file(String::from("src/test/return_for.txt"));
        assert!(res != None);
    }
}