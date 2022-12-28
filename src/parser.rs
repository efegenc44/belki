use crate::token::{ Token, TokenKind, Location };
use crate::ast::Node;

use TokenKind::*;

type ParseError = String;

fn parse_error(msg: String, loc: Location) -> ParseError {
    format!("\n  An error occured while parsing:\n    {}\n    {}\n", loc, msg)  
}

// Grammar
//
// <program>           ::= <declaration>* <End> 
// <declaration>       ::= (<let statement>
//                       | <record declaration>
//                       | <fun declaration>
//                       | <module declaration>
//                       | <statement>) [';']
// <statement>         ::= (<block>
//                       | <return statement>
//                       | <break statement>
//                       | <continue statement>
//                       | <if statement>
//                       | <while statement>
//                       | <import statement>
//                       | <for statement>
//                       | <expression>) [';']
// <module declaration> ::= 'module' <identifier> <block> 
// <block>              ::= '{' <statement>* '}' 
// <for statement>      ::= 'for' <identifier> ':' <expression> <statement>    
// <continue statement> ::= 'continue'
// <break statement>    ::= 'break'
// <import statement>   ::= 'import' <string>
// <let statement>      ::= 'let' <identifier> '=' <expression>
// <return statement>   ::= 'return' <expression>
// <record declaration> ::= 'record' <identifier> '(' <identifier>* ')'
// <fun declaration>    ::= 'fun' <identifier> ['(' (<identifier> ',')* [<identifier>] ')'] (<block> | <expression>) 
// <else>               ::= 'else' <statement>
// <if statement>       ::= 'if' <expression> <statement>
// <while statement>    ::= 'while' <expression> <statement>
// <expression>         ::= <logic or> ['=' <expression>]
// <logic or>           ::= <logic and> ('||' <logic and>)*  
// <logic and>          ::= <type relation> ('&&' <type relation>)*  
// <type relation>      ::= <relation> ('::' <relation>)*
// <relation>           ::= <range> (('==' | '!=' | '<' | '>' | '<=' | '>=') <range>)*
// <range>              ::= <arithmetic> '..' <arithmetic> 
// <arithmetic>         ::= <term> (('+' | '-' | '|>') <term>)*
// <term>               ::= <product> (('*' | '/' | '%') <product>)* 
// <product>            ::= <integer> ['.' [<integer>]]
//                        | <string>
//                        | <identifier>
//                        | 'true'
//                        | 'false'
//                        | 'nothing'
//                        | '[' (<expression> ',')* [<expression>] ']' 
//                        | '(' <expression> ')'
//                        | ('+' | '-' | '!') <product>
//                        | 'fun' ['(' (<identifier> ',')* [<identifier>] ')'] (<block> | <expression>)
//                        | 'if' <expression> 'then' <expression> 'else' <expression>
//                        | '#' '[' (<expression> ':' <expression> ',')* [<expression> ':' <expression>] ']'
//                        | <product> '.' <identifier>
//                        | <product> ?no newline? '(' (<expression>',')* [<expression>] ')'
//                        | <product> ?no newline? '[' <expression> ']' 

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    in_function: usize,
    in_loop: usize,
}

impl Parser {
    pub fn new() -> Parser {
        Parser { tokens: vec![], current: 0, in_function: 0, in_loop: 0 }
    }

    fn next(&mut self) -> Token {
        self.skip_newline();
        self.current += 1;
        self.tokens[self.current - 1].clone()
    }

    fn skip_newline(&mut self) {
        while self.tokens[self.current].kind == TokenKind::NEWLINE {
            self.current += 1;
        }
    }

    fn peek(&mut self) -> Token {
        self.skip_newline();
        self.tokens[self.current].clone()    
    }

    fn peek_with_nl(&self) -> Token {
        self.tokens[self.current].clone()    
    }

    fn expect(&mut self, expected: TokenKind) -> bool {
        if self.peek().kind == expected {
            self.next(); true
        } else {
            false
        }
    }

    fn consume(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        if !self.expect(expected) {
            let peek = self.peek();
            Err(parse_error(
                format!("Expected '{:?}' but got '{}'", expected, peek.text), 
                peek.loc
            ))
        } else {
            Ok(self.tokens[self.current - 1].clone())
        }
    }
    
    fn expression_list(&mut self, stop: TokenKind) -> Result<Vec<Node>, ParseError> {
        let mut exps: Vec<Node> = vec![];
        if !self.expect(stop) {
            exps.push(self.expression()?);
            while !self.expect(stop) {
                self.consume(TokenKind::COMMA)?;
                exps.push(self.expression()?)
            }
        }
        Ok(exps)
    }

    // <product> ::= <integer>
    //             | <float>
    //             | <string>
    //             | <identifier>
    //             | 'true'
    //             | 'false'
    //             | 'nothing'
    //             | '[' (<expression> ',')* [<expression>] ']' 
    //             | '(' <expression> ')'
    //             | ('+' | '-' | '!') <product>
    //             | 'fun' ['(' (<identifier> ',')* [<identifier>] ')'] (<block> | <expression>)
    //             | 'if' <expression> 'then' <expression> 'else' <expression>
    //             | '#' '[' (<expression> ':' <expression> ',')* [<expression> ':' <expression>] ']'
    //             | <product> '.' <identifier>
    //             | <product> ?no newline? '(' (<expression>',')* [<expression>] ')'
    //             | <product> ?no newline? '[' <expression> ']' 

    fn product(&mut self) -> Result<Node, ParseError> {
        let t = self.next();
        let loc = t.loc.clone();
        let mut node = match t.kind {
            INTEGER => {
                if self.peek_with_nl().kind == DOT {
                    self.consume(DOT).unwrap();
                    let mut text = t.text.clone();                    
                    text += ".";
                    if self.peek().kind == INTEGER {
                        let nt = self.next();
                        text += &nt.text; 
                    }
                    return Ok(Node::FloatLiteral(text.parse::<f32>().unwrap(), t.loc))
                }
                Node::IntegerLiteral(t.text.parse::<i32>().unwrap(), t.loc)
            },
            STRING     => Node::StringLiteral(t.text.to_string(), t.loc),
            IDENTIFIER => Node::Identifier(t.text, t.loc),
            TRUE       => Node::True(t.loc),
            FALSE      => Node::False(t.loc),
            NOTHING    => Node::Nothing(t.loc),
            LSQUARE    => Node::ListLiteral(self.expression_list(RSQUARE)?, t.loc),
            LPAREN     => {
                let expr = self.expression()?;
                self.consume(RPAREN)?;
                Node::Group(Box::new(expr), t.loc)
            },
            PLUS | MINUS | BANG => {
                let operand = self.product()?;
                Node::UnaryExpression {
                    op: t.text,
                    operand: Box::new(operand),
                    loc: t.loc
                }
            },
            FUN => {
                self.in_loop += 1;
                let mut args = vec![];
                if self.peek().kind == LPAREN {
                    self.consume(LPAREN)?;
                    if !self.expect(RPAREN) {
                        args.push(self.consume(IDENTIFIER)?.text);
                        while !self.expect(RPAREN) {
                            self.consume(COMMA)?;
                            args.push(self.consume(IDENTIFIER)?.text);
                        }
                    }
                }
                let body;
                if self.peek().kind == LCURLY {
                    body = Box::new(self.block()?);
                } else {
                    body = Box::new(self.expression()?);
                }
                self.in_loop -= 1;
                Node::FunctionDeclaration { name: "".into(), args, body, loc: t.loc }
            },
            IF => {
                let cond = Box::new(self.expression()?);
                self.consume(THEN)?;
                let tru = Box::new(self.expression()?);
                self.consume(ELSE)?;
                let fals = Box::new(self.expression()?);
                Node::IfExpression { cond, tru, fals, loc: t.loc }
            }
            HASH => {
                let mut elements = vec![];
                self.consume(LSQUARE)?;
                if !self.expect(RSQUARE) {
                    let key = self.expression()?;
                    self.consume(COLON)?;
                    let value = self.expression()?;
                    elements.push((key, value));
                    while !self.expect(RSQUARE) {
                        self.consume(COMMA)?;
                        let key = self.expression()?;
                        self.consume(COLON)?;
                        let value = self.expression()?;
                        elements.push((key, value));                    
                    }
                }
                Node::MapLiteral(elements, t.loc)
            }
            
            _ => return Err(parse_error(
                format!("Unexpected Token '{}'", t.text),
                t.loc
            )),
        };
        
        // may be written better
        loop {
            let ntnl = self.peek_with_nl();
            match ntnl.kind {
                LSQUARE => {
                    self.consume(LSQUARE)?;
                    let rhs = self.expression()?;
                    self.consume(RSQUARE)?;
                    node = Node::BinaryExpression { 
                        op: ntnl.text, 
                        lhs: Box::new(node),
                        rhs: Box::new(rhs),
                        loc: loc.clone()
                    }; continue;
                },
                LPAREN => {
                    self.consume(LPAREN)?;
                    let args = self.expression_list(RPAREN)?;
                    node = Node::Application { 
                        fun: Box::new(node), 
                        args,
                        loc: loc.clone()
                    }; continue;
                },
                _ => {}
            }     
            let nt = self.peek();
            match nt.kind {
                DOT => {
                    self.consume(DOT)?;
                    let it = self.consume(IDENTIFIER)?;
                    let rhs = Node::Identifier(
                        it.text, it.loc
                    );

                    node = Node::BinaryExpression { 
                        op: nt.text, 
                        lhs: Box::new(node),
                        rhs: Box::new(rhs), 
                        loc: loc.clone()
                    }; continue;
                }
                _ => break
            }
        }
        Ok(node)
    }

    // <term> ::= <product> (('*' | '/' | '%') <product>)* 
    fn term(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let mut lhs = self.product()?;
        while let STAR | SLASH | PERCENT = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.product()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs), loc: loc.clone()
            };
        } 
        Ok(lhs)
    }

    // <arithmetic> ::= <term> (('+' | '-' | '|>') <term>)*
    pub fn arithmetic(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let mut lhs = self.term()?;
        while let PLUS | MINUS | PIPE = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.term()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs), loc: loc.clone()
            };
        }
        Ok(lhs)
    }
    
    // <range> ::= <arithmetic> '..' <arithmetic> 
    pub fn range(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let lhs = self.arithmetic()?;
        match self.peek().kind {
            TWODOT => {
                self.consume(TWODOT).unwrap();
                let rhs = self.arithmetic()?;
                Ok(Node::BinaryExpression { 
                    op: "..".into(), lhs: Box::new(lhs), rhs: Box::new(rhs), loc 
                })
            },
            _ => Ok(lhs)
        }
    }

    // <relation> ::= <range> (('==' | '!=' | '<' | '>' | '<=' | '>=') <range>)*
    fn relation(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let mut lhs = self.range()?;
        if let DEQUAL | BANGEQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let mut rhs = self.range()?;
            let mut a = Node::BinaryExpression { 
                op: t.text, rhs: Box::new(rhs.clone()), lhs: Box::new(lhs.clone()), loc: loc.clone()
            };
            while let DEQUAL | BANGEQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL = self.peek().kind {
                let t = self.peek().clone();
                lhs = rhs.clone();
                self.consume(t.kind)?;
                rhs = self.range()?;
                let b = Node::BinaryExpression { 
                    op: t.text, rhs: Box::new(rhs.clone()), lhs: Box::new(lhs.clone()), loc: t.loc
                };
                a = Node::BinaryExpression { op: "&&".into(), lhs: Box::new(a), rhs: Box::new(b), loc: loc.clone() }; 
            }
            Ok(a)
        } else {
            Ok(lhs)
        }
    }

    // <type relation> ::= <relation> ('::' <relation>)*
    pub fn type_relation(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let mut lhs = self.relation()?;
        while let DCOLON = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.relation()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs), loc: loc.clone()
            };
        } Ok(lhs)
    }

    // <logic and> ::= <type relation> ('&&' <type relation>)*  
    pub fn logic_and(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let mut lhs = self.type_relation()?;
        while let DAMPERSAND = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.type_relation()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs), loc: loc.clone()
            };
        }
        Ok(lhs)
    }

    // <logic or> ::= <logic and> ('||' <logic and>)*  
    pub fn logic_or(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let mut lhs = self.logic_and()?;
        while let DVLINE = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.logic_and()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs), loc: loc.clone()
            };
        }
        Ok(lhs)
    }

    // <expression> ::= <logic or> ['=' <expression>]
    pub fn expression(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let lhs = self.logic_or()?;
        match self.peek().kind {
            EQUAL => {
                self.consume(EQUAL)?;
                let rhs = self.expression()?;
                Ok(Node::BinaryExpression { op: "=".into(), lhs: Box::new(lhs), rhs: Box::new(rhs), loc })
            }
            _ => Ok(lhs)
        }
    }

    // <while statement> ::= 'while' <expression> <statement>
    fn while_statement(&mut self) -> Result<Node, ParseError> {
        self.in_loop += 1;
        let loc = self.consume(WHILE)?.loc;
        let expr = Box::new(self.expression()?);
        let body = Box::new(self.statement()?);
        self.in_loop -= 1;
        Ok(Node::WhileStatement { expr, body, loc })
    }

    // <if statement> ::= 'if' <expression> <statement> [<else>]
    fn if_statement(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(IF)?.loc;
        Ok(Node::IfStatement { 
            expr: Box::new(self.expression()?), 
            body: Box::new(self.statement()?), 
            els: if self.peek().kind == ELSE { Box::new(self.els()?) } else { Box::new(Node::Nothing(loc.clone())) },
            loc 
        })
    }

    // <else> ::= 'else' <statement>
    fn els(&mut self) -> Result<Node, ParseError> {
        self.consume(ELSE)?;
        self.statement()
    }

    // <fun declaration> ::= 'fun' <identifier> ['(' (<identifier> ',')* [<identifier>] ')'] (<block> | <expression>) 
    fn fun_declaration(&mut self) -> Result<Node, ParseError> {
        self.in_function += 1;
        let loc = self.consume(FUN)?.loc;
        let name = self.consume(IDENTIFIER)?.text;
        let mut args = vec![];
        if self.peek().kind == LPAREN {
            self.consume(LPAREN)?;
            if !self.expect(RPAREN) {
                args.push(self.consume(IDENTIFIER)?.text);
                while !self.expect(RPAREN) {
                    self.consume(COMMA)?;
                    args.push(self.consume(IDENTIFIER)?.text);
                }
            }
        }
        let body;
        if self.peek().kind == LCURLY {
            body = Box::new(self.block()?);
        } else {
            body = Box::new(self.expression()?);
        }
        self.in_function -= 1;
        Ok(Node::FunctionDeclaration { name, args, body, loc })
    }
    
    // <record declaration> ::= 'record' <identifier> '(' <identifier>* ')'
    fn record_declaration(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(RECORD)?.loc;
        let name = self.consume(IDENTIFIER)?.text;
        self.consume(LPAREN)?;
        let mut members = vec![];
        if !self.expect(RPAREN) {
            members.push(self.consume(IDENTIFIER)?.text);
            while !self.expect(RPAREN) {
                self.consume(COMMA)?;
                members.push(self.consume(IDENTIFIER)?.text);
            }
        }
        Ok(Node::RecordDeclaration { name, members, loc })
    }

    // <return statement> ::= 'return' <expression>
    fn return_statement(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(RETURN)?.loc;
        if !(self.in_function > 0) {
            return Err(parse_error("Return outside of a function".to_string(), loc));
        }

        Ok(Node::Return(Box::new(self.expression()?), loc))
    }

    // <let statement> ::= 'let' <identifier> '=' <expression>
    fn let_statement(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(LET)?.loc;
        let name = self.consume(IDENTIFIER)?.text;
        self.consume(EQUAL)?;
        let expr = Box::new(self.expression()?);
        Ok(Node::LetStatement { name, expr, loc })  
    }

    // <import statement> ::= 'import' <string>
    fn import_statement(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(IMPORT)?.loc;
        Ok(Node::Import(self.consume(STRING)?.text, loc))
    }

    // <break statement> ::= 'break'
    fn break_statement(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(BREAK)?.loc;
        if !(self.in_loop > 0) {
            return Err(parse_error("Break outside of a loop".to_string(), loc));
        }

        Ok(Node::Break(loc))
    }

    // <continue statement> ::= 'continue'
    fn continue_statement(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(CONTINUE)?.loc;
        if !(self.in_loop > 0) {
            return Err(parse_error("Continue outside of a loop".to_string(), loc));
        }

        Ok(Node::Continue(loc))
    }

    // <for statement> ::= 'for' <identifier> ':' <expression> <statement>
    fn for_statement(&mut self) -> Result<Node, ParseError> {
        self.in_loop += 1;
        let loc = self.consume(FOR)?.loc;
        let var = self.consume(IDENTIFIER)?.text;
        self.consume(COLON)?;
        let iter = Box::new(self.expression()?);
        let body = Box::new(self.statement()?);
        self.in_loop -= 1;
        Ok(Node::ForStatement { var, iter, body, loc })
    }

    // <block> ::= '{' <statement>* '}' 
    fn block(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(LCURLY)?.loc;
        let mut statements: Vec<Node> = vec![];
        while !self.expect(RCURLY) {
            statements.push(self.declaration()?);
        }
        Ok(Node::Block(statements, loc))
    }

    // <module declaration> ::= 'module' <identifier> <block> 
    fn module_declaration(&mut self) -> Result<Node, ParseError> {
        let loc = self.consume(MODULE)?.loc;
        let name = self.consume(IDENTIFIER)?.text;
        let body;
        if let Node::Block(s, loc) = self.block()? {
            body = Box::new(Node::Module(s, loc));
        } else {unreachable!()}
        Ok(Node::ModuleDeclaration { name, body, loc })
    }

    // <statement> ::= (<block>
    //               | <return statement>
    //               | <break statement>
    //               | <continue statement>
    //               | <if statement>
    //               | <while statement>
    //               | <import statement>
    //               | <for statement>
    //               | <expression>) [';']
    fn statement(&mut self) -> Result<Node, ParseError> {
        let node = match self.peek().kind {
            LCURLY   => self.block()?,
            RETURN   => self.return_statement()?,
            BREAK    => self.break_statement()?,
            CONTINUE => self.continue_statement()?,
            IF       => self.if_statement()?,
            WHILE    => self.while_statement()?,
            IMPORT   => self.import_statement()?,
            FOR      => self.for_statement()?,
            _        => self.expression()?,
        };
        // Optional semi-colon
        self.expect(SEMICOLON);
        Ok(node)
    }
    
    // <declaration> ::= (<let statement>
    //                 | <record declaration>
    //                 | <fun declaration>
    //                 | <module declaration>
    //                 | <statement>) [';']
    fn declaration(&mut self) -> Result<Node, ParseError> {
        let node = match self.peek().kind {
            LET      => self.let_statement()?,
            RECORD   => self.record_declaration()?,
            FUN      => self.fun_declaration()?,
            MODULE   => self.module_declaration()?,
            _        => self.statement()?,
        };
        // Optional semi-colon
        self.expect(SEMICOLON);
        Ok(node)
    }

    // <program> ::= <declaration>* <End> 
    fn module(&mut self) -> Result<Node, ParseError> {
        let loc = self.peek().loc;
        let mut statements = vec![];
        while !self.expect(END) {
            statements.push(self.declaration()?);
        }
        Ok(Node::Module(statements, loc))
    }   

    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Node, ParseError> {
        self.tokens = tokens;
        self.current = 0;
        self.module()
    }

}