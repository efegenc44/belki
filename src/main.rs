use std::env;
use std::io::{ stdin, stdout, Write };
use std::fs;

#[allow(unused_imports)]
use std::thread;

mod math_module;
mod core_module;

mod token;
mod lexer;
mod parser;
mod ast;
mod value;
mod interpreter;
mod error;

fn repl() {
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.init();
    loop {
        let mut line = String::new();
        print!("> "); 
        let _ = stdout().flush();
        let _ = stdin().read_line(&mut line).unwrap();
        line = line.trim().to_string();
        if line == ".quit" {
            break
        } 
    
        // Lexing.
        let mut lexer = lexer::Lexer::new(line);
        let tokens = lexer.tokens();
    
        // Parsing
        let mut parser = parser::Parser::new(tokens);
        let node = parser.parse();
        // node.print(0);
    
        // Evaluating
        interpreter.eval(node);
    }
}

fn from_file(path: String) {
    let source = fs::read_to_string(path)
        .expect("File read error");     
    
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.tokens();

    let mut parser = parser::Parser::new(tokens);
    let node = parser.parse();
    // node.print(0);
    
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.init();
    
    // let child = thread::Builder::new().stack_size(32 * 1024 * 1024).spawn(move || { 
    //     interpreter.eval(node);
    // }).unwrap(); 

    // child.join().unwrap();
    interpreter.eval(node);
}

fn main() {
    let args: Vec<String> = env::args().collect(); 
    let len = args.len();
    if len == 1 {
        repl();
    } 
    else if len == 2 {
        from_file(args[1].clone());
    } 
    else {
        println!("Usage: ./main <file-path>");
    }
}
