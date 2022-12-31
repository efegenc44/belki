#[cfg(test)]
mod tests {
    #[test]
    fn factorial() {
        let res = crate::from_file(String::from("tests/fact.belki"));
        assert!(res != None);
    }
    
    #[test]
    fn fibonacci() {
        let res = crate::from_file(String::from("tests/fib.belki"));
        assert!(res != None);
    }
    
    #[test]
    fn math_lib() {
        let res = crate::from_file(String::from("tests/math_lib.belki"));
        assert!(res != None);
    }

    #[test]
    fn some_test() {
        let res = crate::from_file(String::from("tests/some_test.belki"));
        assert!(res != None);
    }

    #[test]
    fn closure() {
        let res = crate::from_file(String::from("tests/closure.belki"));
        assert!(res != None);
    }
    
    #[test]
    fn import_class_mix() {
        let res = crate::from_file(String::from("tests/import_class_mix.belki"));
        assert!(res != None);
    }

    #[test]
    fn while_loop() {
        let res = crate::from_file(String::from("tests/while_loop.belki"));
        assert!(res != None);
    }

    #[test]
    fn fizzbuzz_step() {
        let res = crate::from_file(String::from("tests/fizzbuzz_step.belki"));
        assert!(res != None);
    }

    #[test]
    fn return_while() {
        let res = crate::from_file(String::from("tests/return_while.belki"));
        assert!(res != None);
    }

    #[test]
    fn return_if() {
        let res = crate::from_file(String::from("tests/return_if.belki"));
        assert!(res != None);
    }

    #[test]
    fn return_block() {
        let res = crate::from_file(String::from("tests/return_block.belki"));
        assert!(res != None);
    }

    #[test]
    fn newline_awareness() {
        let res = crate::from_file(String::from("tests/newline_awareness.belki"));
        assert!(res != None);
    }

    #[test]
    fn for_test() {
        let res = crate::from_file(String::from("tests/for_test.belki"));
        assert!(res != None);
    }

    #[test]
    fn while_break() {
        let res = crate::from_file(String::from("tests/while_break.belki"));
        assert!(res != None);
    }

    #[test]
    fn module_test() {
        let res = crate::from_file(String::from("tests/module_test.belki"));
        assert!(res != None);
    }
    
    #[test]
    fn map() {
        let res = crate::from_file(String::from("tests/map.belki"));
        assert!(res != None);
    }

    #[test]
    fn error() {
        let res = crate::from_file(String::from("tests/error.belki"));
        assert!(res != None);
    }

    #[test]
    fn ifexpr() {
        let res = crate::from_file(String::from("tests/ifexpr.belki"));
        assert!(res != None);
    }

    #[test]
    fn chained_relation() {
        let res = crate::from_file(String::from("tests/chained_relation.belki"));
        assert!(res != None);
    }

    #[test]
    fn return_for() {
        let res = crate::from_file(String::from("tests/return_for.belki"));
        assert!(res != None);
    }
}