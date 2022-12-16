use std::io::{ stdout, Write };

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Program(Vec<Node>),
    Block(Vec<Node>),
    ModuleDeclaration {
        name: String,
        body: Box<Node>
    },
    Import(String),
    MapLiteral(Vec<(Node, Node)>),
    ForStatement {
        var: String,
        iter: Box<Node>,
        body: Box<Node>
    },
    LetStatement {
        name: String, 
        expr: Box<Node>
    },
    Return(Box<Node>),
    Break,
    Continue,
    RecordDeclaration { 
        name: String, 
        members: Vec<String>, 
    },
    FunctionDeclaration { 
        name: String, 
        args: Vec<String>, 
        body: Box<Node>
    },
    IfStatement {
        expr: Box<Node>,
        body: Box<Node>,
        els:  Box<Node> 
    },
    IfExpression {
        cond: Box<Node>,
        tru: Box<Node>,
        fals: Box<Node> 
    },
    WhileStatement {
        expr: Box<Node>,
        body: Box<Node>,
    },
    BinaryExpression {
        op: String,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    IntegerLiteral(i32),
    FloatLiteral(f32),
    ListLiteral(Vec<Node>),
    StringLiteral(String),
    Identifier(String),
    Group(Box<Node>),
    UnaryExpression {
        op: String,
        operand: Box<Node>
    },
    Application {
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