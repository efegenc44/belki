#[allow(unused_imports)]
use std::io::{ stdout, Write };

use crate::token::Location;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Module(Vec<Node>, Location),
    Block(Vec<Node>, Location),
    ModuleDeclaration {
        name: String,
        body: Box<Node>,
        loc: Location
    },
    Import(String, Location),
    MapLiteral(Vec<(Node, Node)>, Location),
    ForStatement {
        var: String,
        iter: Box<Node>,
        body: Box<Node>,
        loc: Location
    },
    LetStatement {
        name: String, 
        expr: Box<Node>,
        loc: Location
    },
    Return(Box<Node>, Location),
    Break(Location),
    Continue(Location),
    RecordDeclaration { 
        name: String, 
        members: Vec<String>, 
        loc: Location
    },
    FunctionDeclaration { 
        name: String, 
        args: Vec<String>, 
        body: Box<Node>,
        loc: Location
    },
    IfStatement {
        expr: Box<Node>,
        body: Box<Node>,
        els:  Box<Node>,
        loc:  Location 
    },
    IfExpression {
        cond: Box<Node>,
        tru:  Box<Node>,
        fals: Box<Node>,
        loc:  Location 
    },
    WhileStatement {
        expr: Box<Node>,
        body: Box<Node>,
        loc:  Location
    },
    BinaryExpression {
        op:  String,
        lhs: Box<Node>,
        rhs: Box<Node>,
        loc: Location
    },
    IntegerLiteral(i32, Location),
    FloatLiteral(f32, Location),
    ListLiteral(Vec<Node>, Location),
    StringLiteral(String, Location),
    Identifier(String, Location),
    Group(Box<Node>, Location),
    UnaryExpression {
        op: String,
        operand: Box<Node>,
        loc: Location
    },
    Application {
        fun: Box<Node>,
        args: Vec<Node>,
        loc: Location 
    },
    True(Location),
    False(Location),
    Nothing(Location),
}

impl Node {
    // :D yes
    pub fn get_loc(&self) -> Location {
        match self {
            Node::Module(_, loc)                                           => loc.clone(),
            Node::Block(_, loc)                                            => loc.clone(),
            Node::ModuleDeclaration { name: _, body: _, loc }              => loc.clone(),
            Node::Import(_, loc)                                           => loc.clone(),
            Node::MapLiteral(_, loc)                                       => loc.clone(),
            Node::ForStatement { var: _, iter: _, body: _, loc }           => loc.clone(),
            Node::LetStatement { name: _ , expr: _, loc }                  => loc.clone(),
            Node::Return(_, loc)                                           => loc.clone(),
            Node::Break(loc)                                               => loc.clone(),
            Node::Continue(loc)                                            => loc.clone(),
            Node::RecordDeclaration { name: _, members: _, loc }           => loc.clone(),
            Node::FunctionDeclaration { name: _, args: _, body: _, loc }   => loc.clone(),
            Node::IfStatement { expr: _, body: _, els: _, loc }            => loc.clone(),
            Node::IfExpression { cond: _, tru: _, fals: _, loc }           => loc.clone(),
            Node::WhileStatement { expr: _, body: _, loc }                 => loc.clone(),
            Node::BinaryExpression { op: _, lhs: _, rhs: _, loc }          => loc.clone(),
            Node::IntegerLiteral(_, loc)                                   => loc.clone(),
            Node::FloatLiteral(_, loc)                                     => loc.clone(),
            Node::ListLiteral(_, loc)                                      => loc.clone(),
            Node::StringLiteral(_, loc)                                    => loc.clone(),
            Node::Identifier(_, loc)                                       => loc.clone(),
            Node::Group(_, loc)                                            => loc.clone(),
            Node::UnaryExpression { op: _, operand: _, loc }               => loc.clone(),
            Node::Application { fun: _, args: _, loc }                     => loc.clone(),
            Node::True(loc)                                                => loc.clone(),
            Node::False(loc)                                               => loc.clone(),
            Node::Nothing(loc)                                             => loc.clone(),
        }
    }
}

/*
#[allow(dead_code)]
impl Node {
    pub fn print(&self, indent: usize) {
        
        fn print_spaces(indent: usize) {
            for _ in 0..indent { print!(" ") } 
            let _ = stdout().flush();
        }
        print_spaces(indent);
        match self {
            Node::Block(statements) => {
                println!("block");
                for statement in statements {
                    statement.print(indent + 1);
                }
            },
            Node::Module(statements) => {
                println!("program");
                for statement in statements {
                    statement.print(indent + 1);
                }
            },
            Node::Import(path) => {
                println!("import");
                print_spaces(indent + 1);
                println!("{}", path);
            },
            Node::MapLiteral(map) => {
                println!("map");
                for (key, value) in map {
                    key.print(indent + 1);
                    value.print(indent + 1);
                } 
            },
            Node::ModuleDeclaration { name, body } => {
                println!("module");
                print_spaces(indent + 1);
                println!("{}", name);
                body.print(indent + 1);
            }
            Node::ForStatement { var, iter, body } => {
                println!("for");
                print_spaces(indent + 1);
                println!("{}", var);
                iter.print(indent + 1);
                body.print(indent + 1);
            }
            Node::LetStatement { name, expr } => {
                println!("let");
                print_spaces(indent + 1);
                println!("{}", name);
                expr.print(indent + 1);
            },
            Node::Return(expr) => {
                println!("return");
                expr.print(indent + 1);
            },
            Node::Break => println!("break"),
            Node::Continue => println!("continue"),
            Node::RecordDeclaration { name, members } => {
                println!("class");
                print_spaces(indent + 1);
                println!("{}", name);
                print_spaces(indent + 1);
                for member in members {
                    print!("{} ", member);
                }
                let _ = stdout().flush();
            },
            Node::FunctionDeclaration { name, args, body } => {
                println!("fun");
                print_spaces(indent + 1);
                println!("{}", name);
                print_spaces(indent + 1);
                for arg in args {
                    print!("{} ", arg);
                }
                let _ = stdout().flush();
                println!();
                body.print(indent + 1);
            },
            Node::IfStatement { expr, body, els } => {
                println!("if");
                expr.print(indent + 1);
                body.print(indent + 1);
                els.print(indent + 1);
            },
            Node::IfExpression { cond, tru, fals } => {
                println!("if expr");
                cond.print(indent + 1);
                tru.print(indent + 1);
                fals.print(indent + 1);
            },  
            Node::WhileStatement { expr, body } => {
                println!("while");
                expr.print(indent + 1);
                body.print(indent + 1);
            },
            Node::BinaryExpression { op, lhs, rhs } => {
                println!("binary");
                print_spaces(indent + 1);
                println!("{}", op);
                lhs.print(indent + 2);
                rhs.print(indent + 2);
            },
            Node::ListLiteral(list) => {
                println!("list");
                for element in list {
                    element.print(indent + 1)
                }
            },
            Node::UnaryExpression { op, operand } => {
                println!("unary");    
                print_spaces(indent + 1);
                println!("{}", op);    
                operand.print(indent + 1);
            },
            Node::Application { fun, args } => {
                println!("Fun Call");    
                fun.print(indent + 1);
                for arg in args {
                    arg.print(indent + 1);
                } 
            },
            Node::StringLiteral(s)     => println!("\"{}\"", s),
            Node::Identifier(s) => println!("{}", s),    
            Node::Group(node)   => node.print(indent + 1),    
            Node::IntegerLiteral(i)    => println!("{}", i),
            Node::FloatLiteral(i)      => println!("{}", i),
            Node::True          => println!("true"),       
            Node::False         => println!("false"),      
            Node::Nothing       => println!("Nothing"),        
            Node::None          => println!("None") 
        }
    }

}
*/