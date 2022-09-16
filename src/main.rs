use std::env;
use std::io::{ stdin, stdout, Write };
use std::fs;
use std::process::exit;

mod math_module;
mod core_module;

mod token;
mod lexer;
mod parser;
mod ast;
mod value;
mod interpreter;

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
    
        let mut lexer = lexer::Lexer::new(line);
        let tokens = match lexer.tokens() {
            Ok(tokens) => tokens,
            Err(error) => {
                println!("{:?}", error);
                continue;
            }
        };
    
        let mut parser = parser::Parser::new(tokens);
        let node = match parser.parse() {
            Ok(node) => node,
            Err(error) => {
                println!("{:?}", error);
                continue;
            }
        };
        // node.print(0);
    
        match interpreter.eval(node) {
            Ok(_) => {}
            Err(error) => {
                println!("{:?}", error);
                continue;
            }
        }
    }
}

fn from_file(path: String) {
    let source = fs::read_to_string(path)
        .expect("File read error");     
    
    let mut lexer = lexer::Lexer::new(source);
    let tokens = match lexer.tokens() {
        Ok(tokens) => tokens,
        Err(error) => {
            println!("{:?}", error);
            exit(0)
        }
    };

    let mut parser = parser::Parser::new(tokens);
    let node = match parser.parse() {
        Ok(node) => node,
        Err(error) => {
            println!("{:?}", error);
            exit(0)
        }
    };
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.init();
    
    match interpreter.eval(node) {
        Ok(_) => {},
        Err(error) => {
            println!("{:?}", error);
            exit(0)
        }
    }
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
        println!("Usage: ./<name> <file-path>");
    }
}
