use std::process::exit;

use crate::token::{ Token, TokenKind, Location };
use crate::ast::Node;
use crate::error::report;

// Grammar
//
// <program>           ::= <statement>* <End> 
// <statement>         ::= <expression>
//                       | <block>
//                       | <let statement>
//                       | <return statement>
//                       | <class declaration>
//                       | <if statement>
//                       | <while statement>
//                       | <import statement>
// <block>             ::= '{' <statement>* '}' 
// <import statement>  ::= 'import' <identifier>
// <let statement>     ::= 'let' <identifier> '=' <expression>
// <return statement>  ::= 'return' <expression>
// <class declaration> ::= 'class' <identifier> '{' (<identifier> | <fun declaration>)* '}'
// <fun declaration>   ::= 'fun' <identifier> '(' (<identifier> ',')* [<identifier>] ')' <block>
// <else>              ::= 'else' (<block> | <expression> <block> [<else>])
// <if statement>      ::= 'if' <expression> <block> <else>*
// <while statement>   ::= 'while' <expression> <block>
// <expression>        ::= <logic> ['=' <expression>]
// <logic>             ::= <relation> [(&& | ||) <relation>]*  
// <relation>          ::= <arithmetic> [(== | != | < | > | <= | >=) <arithmetic>]  
// <arithmetic>        ::= <term> [(+ | - | |>) <term>]*
// <term>              ::= <product> [(* | / | %) <product>]* 
// <product>           ::= <integer>
//                       | <float>
//                       | <string>
//                       | <identifier>
//                       | '(' <expression> ')'
//                       | (+ | - | !) <product>
//                       | 'true'
//                       | 'false'
//                       | 'nothing'
//                       | <product> '.' <identifier>
//                       | <product> '(' (<expression>',')* [<expression>] ')'
//                       | <product> '[' <expression> ']' 
//                       | '[' (<expression> ',')* [<expression>] ']' 




pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    fn next(&mut self) -> Token {
        self.current += 1;
        self.tokens[self.current - 1].clone()
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()    
    }

    fn get_loc(&self) -> Location {
        self.peek().loc
    }

    fn expect(&mut self, expected: TokenKind) -> bool {
        if self.peek().kind == expected {
            self.next(); true
        } else {
            false
        }
    }

    fn consume(&mut self, expected: TokenKind) -> Token {
        if !self.expect(expected) {
            report(self.get_loc(), format!("Expected {:?}, got {:?}.", expected, self.peek().kind).as_str());
            exit(1);
        } else {
            self.tokens[self.current - 1].clone()
        }
    }
    
    fn product(&mut self) -> Node {
        let t= self.peek();
        let mut node: Node;
        match t.kind {
            TokenKind::INTEGER     => {
                self.consume(TokenKind::INTEGER);
                node = Node::Integer(t.text.parse::<i32>().unwrap());
            }
            TokenKind::FLOAT       => { 
                self.consume(TokenKind::FLOAT);
                node = Node::Float(t.text.parse::<f32>().unwrap()); 
            }
            TokenKind::STRING      => { 
                self.consume(TokenKind::STRING);
                node = Node::String(t.text.to_string());
            }
            TokenKind::IDENTIFIER  => { 
                self.consume(TokenKind::IDENTIFIER);
                node = Node::Identifier(t.text);
            }
            TokenKind::LPAREN  => {
                self.consume(TokenKind::LPAREN);
                let expr = self.expression();
                self.consume(TokenKind::RPAREN);
                node = Node::Group(Box::new(expr));
            },
            TokenKind::PLUS | TokenKind::MINUS | TokenKind::BANG => {
                self.consume(t.kind);
                let operand = self.product();
                node = Node::Unary {
                    op: t.text,
                    operand: Box::new(operand)
                };
            },
            TokenKind::TRUE => { 
                self.consume(TokenKind::TRUE);
                node = Node::True;
            }
            TokenKind::FALSE => { 
                self.consume(TokenKind::FALSE);
                node = Node::False;
            }
            TokenKind::NOTHING => { 
                self.consume(TokenKind::NOTHING);
                node = Node::Nothing;
            }
            TokenKind::LSQUARE => {
                self.consume(TokenKind::LSQUARE);
                let mut elements: Vec<Node> = vec![];
                if !self.expect(TokenKind::RSQUARE) {
                    elements.push(self.expression());
                    while !self.expect(TokenKind::RSQUARE) {
                        self.consume(TokenKind::COMMA);
                        elements.push(self.expression())
                    }
                }
                node = Node::List(elements);
            }            
                
            _ => {
                report(self.get_loc(), "Parse Error");
                node = Node::None;
            } 
        }
        
        loop {
            let nt = self.peek();
            match nt.kind {
                TokenKind::DOT => {
                    self.consume(TokenKind::DOT);
                    let rhs = Node::Identifier(
                        self.consume(TokenKind::IDENTIFIER).text
                    );

                    node = Node::Binary { 
                        op: nt.text, 
                        lhs: Box::new(node),
                        rhs: Box::new(rhs), 
                    }
                },
                TokenKind::LSQUARE => {
                    self.consume(TokenKind::LSQUARE);
                    let rhs = self.expression();
                    self.consume(TokenKind::RSQUARE);

                    node = Node::Binary { 
                        op: nt.text, 
                        lhs: Box::new(node),
                        rhs: Box::new(rhs), 
                    }
                },
                TokenKind::LPAREN => {
                    self.consume(TokenKind::LPAREN);
                    let mut args: Vec<Node> = vec![];
                    if !self.expect(TokenKind::RPAREN) {
                        args.push(self.expression());
                        while !self.expect(TokenKind::RPAREN) {
                            self.consume(TokenKind::COMMA);
                            args.push(self.expression())
                        }
                    }

                    node = Node::FunCall { 
                        fun: Box::new(node), 
                        args: args
                    }
                },
                _ => break
            }     
        }
        node
    }

    fn term(&mut self) -> Node {
        let mut lhs = self.product();
        match self.peek().kind {
            TokenKind::STAR | TokenKind::SLASH | TokenKind::PERCENT => {
                while self.peek().kind == TokenKind::STAR  || 
                      self.peek().kind == TokenKind::SLASH ||
                      self.peek().kind == TokenKind::PERCENT {
                        let op = self.peek().text; 
                        self.consume(self.peek().kind);
                        let rhs = self.product();
                        lhs = Node::Binary {
                            op: op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs)
                        };
                }
                lhs
            },         
            _ => {
                lhs
            }
        }
    }

    pub fn arithmetic(&mut self) -> Node {
        let mut lhs = self.term();
        match self.peek().kind {
            TokenKind::PLUS | TokenKind::MINUS | TokenKind::PIPE => {
                while self.peek().kind == TokenKind::MINUS || 
                      self.peek().kind == TokenKind::PLUS  || 
                      self.peek().kind == TokenKind::PIPE {
                        let op = self.peek().text; 
                        self.consume(self.peek().kind);
                        let rhs = self.term();
                        lhs = Node::Binary {
                            op: op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs)
                        };
                }
                lhs
            },         
            _ => {
                lhs
            }
        }
    }

    fn relation(&mut self) -> Node {
        let lhs = self.arithmetic();
        match self.peek().kind {
            TokenKind::DEQUAL       |
            TokenKind::BANGEQUAL    |
            TokenKind::GREATER      |
            TokenKind::GREATEREQUAL |
            TokenKind::LESS         |
            TokenKind::LESSEQUAL    => {
                let op = self.peek().text;
                self.consume(self.peek().kind);
                let rhs = self.arithmetic();
                Node::Binary { 
                    op: op, 
                    rhs: Box::new(rhs), 
                    lhs: Box::new(lhs) 
                }
            },
            _ => lhs
        }
    }

    pub fn logic(&mut self) -> Node {
        let mut lhs = self.relation();
        match self.peek().kind {
            TokenKind::DVLINE | TokenKind::DAMPERSAND => {
                while self.peek().kind == TokenKind::DVLINE || 
                      self.peek().kind == TokenKind::DAMPERSAND {
                        let op = self.peek().text; 
                        self.consume(self.peek().kind);
                        let rhs = self.relation();
                        lhs = Node::Binary {
                            op: op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs)
                        };
                }
                lhs
            },         
            _ => {
                lhs
            }
        }
    }

    pub fn expression(&mut self) -> Node {
        let lhs = self.logic();
        match self.peek().kind {
            TokenKind::EQUAL => {
                self.consume(TokenKind::EQUAL);
                let rhs = self.expression();
                Node::Binary { op: String::from("="), lhs: Box::new(lhs), rhs: Box::new(rhs) }
            }

            _ => {lhs}
        }
    }

    fn while_statement(&mut self) -> Node {
        self.consume(TokenKind::WHILE);
        Node::While { expr: Box::new(self.expression()), body: Box::new(self.block()) }
    }

    fn if_statement(&mut self) -> Node {
        self.consume(TokenKind::IF);
        Node::If { 
            expr: Box::new(self.expression()), 
            body: Box::new(self.block()), 
            els: if self.peek().kind == TokenKind::ELSE { Box::new(self.els()) } else { Box::new(Node::None) } 
        }
    }

    fn els(&mut self) -> Node {
        self.consume(TokenKind::ELSE);
        if self.peek().kind == TokenKind::LCURLY {
            Node::Else { expr: Box::new(Node::None), body: Box::new(self.block()), els: Box::new(Node::None) }
        } else {
            Node::Else { 
                expr: Box::new(self.expression()), 
                body: Box::new(self.block()), 
                els: if self.peek().kind == TokenKind::ELSE { Box::new(self.els()) } else { Box::new(Node::None) } }
        }
    }

    fn fun_declaration(&mut self) -> Node {
        self.consume(TokenKind::FUN);
        let name = self.consume(TokenKind::IDENTIFIER).text;
        self.consume(TokenKind::LPAREN);
        let mut args: Vec<String> = vec![];
        if !self.expect(TokenKind::RPAREN) {
            args.push(self.consume(TokenKind::IDENTIFIER).text);
            while !self.expect(TokenKind::RPAREN) {
                self.consume(TokenKind::COMMA);
                args.push(self.consume(TokenKind::IDENTIFIER).text);
            }
        }
        let body = Box::new(self.fun_block());
        Node::Fun { name: name, args: args, body: body }
    }

    fn class_declaration(&mut self) -> Node {
        self.consume(TokenKind::CLASS);
        let name = self.consume(TokenKind::IDENTIFIER).text;
        self.consume(TokenKind::LCURLY);
        let mut members: Vec<String> = vec![];
        let mut methods: Vec<Node> = vec![];
        while !self.expect(TokenKind::RCURLY) {
            match self.peek().kind {
                TokenKind::FUN => methods.push(self.fun_declaration()),
                TokenKind::IDENTIFIER => members.push(self.consume(TokenKind::IDENTIFIER).text),
                _ => println!("error")
            }
        }
        Node::Class { name: name, members: members, methods: methods }
    }

    fn return_statement(&mut self) -> Node {
        self.consume(TokenKind::RETURN);
        Node::Return(Box::new(self.expression()))
    }

    fn let_statement(&mut self) -> Node {
        self.consume(TokenKind::LET);
        let name = self.consume(TokenKind::IDENTIFIER).text;
        self.consume(TokenKind::EQUAL);
        let expr = Box::new(self.expression());
        self.expect(TokenKind::SEMICOLON);
        Node::Let { name: name, expr: expr }  
    }

    fn import_statement(&mut self) -> Node {
        self.consume(TokenKind::IMPORT);
        Node::Import(self.consume(TokenKind::STRING).text)
    }

    fn print_statement(&mut self) -> Node {
        self.consume(TokenKind::PRINT);
        Node::Print(Box::new(self.expression()))    
    }

    fn block(&mut self) -> Node {
        self.consume(TokenKind::LCURLY);
        let mut statements: Vec<Node> = vec![];
        while !self.expect(TokenKind::RCURLY) {
            statements.push(self.statement());
        }
        Node::Block(statements)
    }

    fn fun_block(&mut self) -> Node {
        self.consume(TokenKind::LCURLY);
        let mut statements: Vec<Node> = vec![];
        while !self.expect(TokenKind::RCURLY) {
            statements.push(self.statement());
        }
        Node::FunBlock(statements)
    }

    fn statement(&mut self) -> Node {
        match self.peek().kind {
            TokenKind::LCURLY => self.block(),
            TokenKind::LET    => self.let_statement(),
            TokenKind::RETURN => self.return_statement(),
            TokenKind::CLASS  => self.class_declaration(),
            TokenKind::FUN    => self.fun_declaration(),
            TokenKind::IF     => self.if_statement(),
            TokenKind::WHILE  => self.while_statement(),
            TokenKind::IMPORT => self.import_statement(),
            TokenKind::PRINT  => self.print_statement(),
            _                 => self.expression()
        }
    }

    fn program(&mut self) -> Node {
        let mut statements: Vec<Node>= vec![];
        while !self.expect(TokenKind::END) {
            statements.push(self.statement());
        }
        Node::Program(statements)
    }   

    pub fn parse(&mut self) -> Node {
        self.program()
    }

}
