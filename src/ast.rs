use std::io::{ stdout, Write };

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Program(Vec<Node>),
    Block(Vec<Node>),
    Block2(Vec<Node>),
    FunBlock(Vec<Node>), // Hack, I guess
    Module(String, Box<Node>),
    Import(String),
    MapLit(Vec<(Node, Node)>),
    For {
        var: String,
        iter: Box<Node>,
        body: Box<Node>
    },
    Let {
        name: String, 
        expr: Box<Node>
    },
    Return(Box<Node>),
    Break,
    Continue,
    Class { 
        name: String, 
        members: Vec<String>, 
    },
    Fun { 
        name: String, 
        args: Vec<String>, 
        body: Box<Node>
    },
    If {
        expr: Box<Node>,
        body: Box<Node>,
        els:  Box<Node> 
    },
    IfExpr {
        cond: Box<Node>,
        tru: Box<Node>,
        fals: Box<Node> 
    },
    Else {
        body: Box<Node>,
    },
    While {
        expr: Box<Node>,
        body: Box<Node>,
    },
    Binary {
        op: String,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Integer(i32),
    Float(f32),
    List(Vec<Node>),
    String(String),
    Identifier(String),
    Group(Box<Node>),
    Unary {
        op: String,
        operand: Box<Node>
    },
    FunCall {
        fun: Box<Node>,
        args: Vec<Node> 
    },
    True,
    False,
    Nothing,
    None
}

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
            Node::Block2(statements) => {
                println!("block");
                for statement in statements {
                    statement.print(indent + 1);
                }
            },
            Node::FunBlock(statements) => {
                println!("block");
                for statement in statements {
                    statement.print(indent + 1);
                }
            },
            Node::Program(statements) => {
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
            Node::MapLit(map) => {
                println!("map");
                for (key, value) in map {
                    key.print(indent + 1);
                    value.print(indent + 1);
                } 
            },
            Node::Module(name, body) => {
                println!("module");
                print_spaces(indent + 1);
                println!("{}", name);
                body.print(indent + 1);
            }
            Node::For { var, iter, body } => {
                println!("for");
                print_spaces(indent + 1);
                println!("{}", var);
                iter.print(indent + 1);
                body.print(indent + 1);
            }
            Node::Let { name, expr } => {
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
            Node::Class { name, members } => {
                println!("class");
                print_spaces(indent + 1);
                println!("{}", name);
                print_spaces(indent + 1);
                for member in members {
                    print!("{} ", member);
                }
                let _ = stdout().flush();
            },
            Node::Fun { name, args, body } => {
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
            Node::If { expr, body, els } => {
                println!("if");
                expr.print(indent + 1);
                body.print(indent + 1);
                els.print(indent + 1);
            },
            Node::IfExpr { cond, tru, fals } => {
                println!("if expr");
                cond.print(indent + 1);
                tru.print(indent + 1);
                fals.print(indent + 1);
            },
            Node::Else { body } => {
                println!("else");
                body.print(indent + 1);
            },  
            Node::While { expr, body } => {
                println!("while");
                expr.print(indent + 1);
                body.print(indent + 1);
            },
            Node::Binary { op, lhs, rhs } => {
                println!("binary");
                print_spaces(indent + 1);
                println!("{}", op);
                lhs.print(indent + 2);
                rhs.print(indent + 2);
            },
            Node::List(list) => {
                println!("list");
                for element in list {
                    element.print(indent + 1)
                }
            },
            Node::Unary { op, operand } => {
                println!("unary");    
                print_spaces(indent + 1);
                println!("{}", op);    
                operand.print(indent + 1);
            },
            Node::FunCall { fun, args } => {
                println!("Fun Call");    
                fun.print(indent + 1);
                for arg in args {
                    arg.print(indent + 1);
                } 
            },
            Node::String(s)     => println!("\"{}\"", s),
            Node::Identifier(s) => println!("{}", s),    
            Node::Group(node)   => node.print(indent + 1),    
            Node::Integer(i)    => println!("{}", i),
            Node::Float(i)      => println!("{}", i),
            Node::True          => println!("true"),       
            Node::False         => println!("false"),      
            Node::Nothing       => println!("Nothing"),        
            Node::None          => println!("None") 
        }
    }

}