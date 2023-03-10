use std::env;
use std::io::{ stdin, stdout, Write };
use std::fs;
use std::process::exit;

use interpreter::State;

mod math_module;
mod core_module;

mod token;
mod lexer;
mod parser;
mod ast;
mod value;
mod interpreter;
mod tests;

fn repl() {
    let mut interpreter = interpreter::Interpreter::new();
    let mut lexer = lexer::Lexer::new("REPL".to_string());
    let mut parser = parser::Parser::new();
    interpreter.init();
    interpreter.repl_mode();
    loop {
        let mut line = String::new();
        print!("belki > "); 
        let _ = stdout().flush();
        let _ = stdin().read_line(&mut line).unwrap();
        line = line.trim_end().to_string();
        if line == ".quit" {
            break
        } 
    
        let tokens = match lexer.tokens(line.clone()) {
            Ok(tokens) => tokens,
            Err(error) => {
                println!("{}", error);
                continue;
            }
        };
    
        let node = match parser.parse(tokens) {
            Ok(node) => node,
            Err(error) => {
                println!("{}", error);
                continue;
            }
        };

        match interpreter.eval(node) {
            Ok(_) => {}
            Err(State::Error(error)) => {
                println!("{}", error); continue; 
            },
            _ => unreachable!(),
        }
    }
}

fn from_file(path: String) -> Option<()> {
    let source = fs::read_to_string(path.clone())
        .expect("\n  Error while reading the file\n");     
    
    let mut lexer = lexer::Lexer::new(path);
    let tokens = match lexer.tokens(source) {
        Ok(tokens) => tokens,
        Err(error) => {
            println!("{}", error); 
            return None;
        }
    };

    let mut parser = parser::Parser::new();
    let node = match parser.parse(tokens) {
        Ok(node) => node,
        Err(error) => {
            println!("{}", error); 
            return None;
        }
    };

    let mut interpreter = interpreter::Interpreter::new();
    interpreter.init();
    
    match interpreter.eval(node) {
        Ok(_) => Some(()),
        Err(State::Error(error)) => {
            println!("{}", error); None 
        },
        _ => unreachable!(),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect(); 
    let len = args.len();
    if len == 1 {
        repl();
    } 
    else if len == 2 {
        if let None = from_file(args[1].clone()) {
            exit(0);
        }
    } 
    else {
        println!("\n  Usage: ./belki <file-path>");
    }
}
