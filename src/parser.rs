use crate::token::{ Token, TokenKind, Location };
use crate::ast::Node;

use TokenKind::*;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Location),
    IllDefinedAST(Location)
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(loc) => write!(f, "\n  An error occures while parsing:\n      Unexpected Token at {}\n", loc),
            Self::IllDefinedAST(loc)   => write!(f, "\n  An error occures while parsing:\n      Ill Defined AST at {}\n", loc),
        }
    }
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
}

impl Parser {
    pub fn new() -> Parser {
        Parser { tokens: vec![], current: 0 }
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
            Err(ParseError::UnexpectedToken(self.peek().loc))
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
                    return Ok(Node::FloatLiteral(text.parse::<f32>().unwrap()))
                }
                Node::IntegerLiteral(t.text.parse::<i32>().unwrap())
            },
            STRING     => Node::StringLiteral(t.text.to_string()),
            IDENTIFIER => Node::Identifier(t.text),
            TRUE       => Node::True,
            FALSE      => Node::False,
            NOTHING    => Node::Nothing,
            LSQUARE    => Node::ListLiteral(self.expression_list(RSQUARE)?),
            LPAREN     => {
                let expr = self.expression()?;
                self.consume(RPAREN)?;
                Node::Group(Box::new(expr))
            },
            PLUS | MINUS | BANG => {
                let operand = self.product()?;
                Node::UnaryExpression {
                    op: t.text,
                    operand: Box::new(operand)
                }
            },
            FUN => {
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
                Node::FunctionDeclaration { name: "".into(), args, body }
            },
            IF => {
                let cond = Box::new(self.expression()?);
                self.consume(THEN)?;
                let tru = Box::new(self.expression()?);
                self.consume(ELSE)?;
                let fals = Box::new(self.expression()?);
                Node::IfExpression { cond, tru, fals }
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
                Node::MapLiteral(elements)
            }
            
            _ => return Err(ParseError::IllDefinedAST(t.loc)),
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
                    }; continue;
                },
                LPAREN => {
                    self.consume(LPAREN)?;
                    let args = self.expression_list(RPAREN)?;
                    node = Node::Application { 
                        fun: Box::new(node), 
                        args
                    }; continue;
                },
                _ => {}
            }     
            let nt = self.peek();
            match nt.kind {
                DOT => {
                    self.consume(DOT)?;
                    let rhs = Node::Identifier(
                        self.consume(IDENTIFIER)?.text
                    );

                    node = Node::BinaryExpression { 
                        op: nt.text, 
                        lhs: Box::new(node),
                        rhs: Box::new(rhs), 
                    }; continue;
                }
                _ => break
            }
        }
        Ok(node)
    }

    // <term> ::= <product> (('*' | '/' | '%') <product>)* 
    fn term(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.product()?;
        while let STAR | SLASH | PERCENT = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.product()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs)
            };
        } 
        Ok(lhs)
    }

    // <arithmetic> ::= <term> (('+' | '-' | '|>') <term>)*
    pub fn arithmetic(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.term()?;
        while let PLUS | MINUS | PIPE = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.term()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs)
            };
        }
        Ok(lhs)
    }
    
    // <range> ::= <arithmetic> '..' <arithmetic> 
    pub fn range(&mut self) -> Result<Node, ParseError> {
        let lhs = self.arithmetic()?;
        match self.peek().kind {
            TWODOT => {
                self.consume(TWODOT).unwrap();
                let rhs = self.arithmetic()?;
                Ok(Node::BinaryExpression { 
                    op: "..".into(), lhs: Box::new(lhs), rhs: Box::new(rhs) 
                })
            },
            _ => Ok(lhs)
        }
    }

    // <relation> ::= <range> (('==' | '!=' | '<' | '>' | '<=' | '>=') <range>)*
    fn relation(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.range()?;
        if let DEQUAL | BANGEQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let mut rhs = self.range()?;
            let mut a = Node::BinaryExpression { 
                op: t.text, rhs: Box::new(rhs.clone()), lhs: Box::new(lhs.clone()) 
            };
            while let DEQUAL | BANGEQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL = self.peek().kind {
                let t = self.peek().clone();
                lhs = rhs.clone();
                self.consume(t.kind)?;
                rhs = self.range()?;
                let b = Node::BinaryExpression { 
                    op: t.text, rhs: Box::new(rhs.clone()), lhs: Box::new(lhs.clone()) 
                };
                a = Node::BinaryExpression { op: "&&".into(), lhs: Box::new(a), rhs: Box::new(b) }; 
            }
            Ok(a)
        } else {
            Ok(lhs)
        }
    }

    // <type relation> ::= <relation> ('::' <relation>)*
    pub fn type_relation(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.relation()?;
        while let DCOLON = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.relation()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs)
            };
        } Ok(lhs)
    }

    // <logic and> ::= <type relation> ('&&' <type relation>)*  
    pub fn logic_and(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.type_relation()?;
        while let DAMPERSAND = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.type_relation()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs)
            };
        }
        Ok(lhs)
    }

    // <logic or> ::= <logic and> ('||' <logic and>)*  
    pub fn logic_or(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.logic_and()?;
        while let DVLINE = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.logic_and()?;
            lhs = Node::BinaryExpression {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs)
            };
        }
        Ok(lhs)
    }

    // <expression> ::= <logic or> ['=' <expression>]
    pub fn expression(&mut self) -> Result<Node, ParseError> {
        let lhs = self.logic_or()?;
        match self.peek().kind {
            EQUAL => {
                self.consume(EQUAL)?;
                let rhs = self.expression()?;
                Ok(Node::BinaryExpression { op: "=".into(), lhs: Box::new(lhs), rhs: Box::new(rhs) })
            }
            _ => Ok(lhs)
        }
    }

    // <while statement> ::= 'while' <expression> <statement>
    fn while_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(WHILE)?;
        Ok(Node::WhileStatement { expr: Box::new(self.expression()?), body: Box::new(self.statement()?) })
    }

    // <if statement> ::= 'if' <expression> <statement> [<else>]
    fn if_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(IF)?;
        Ok(Node::IfStatement { 
            expr: Box::new(self.expression()?), 
            body: Box::new(self.statement()?), 
            els: if self.peek().kind == ELSE { Box::new(self.els()?) } else { Box::new(Node::None) } 
        })
    }

    // <else> ::= 'else' <statement>
    fn els(&mut self) -> Result<Node, ParseError> {
        self.consume(ELSE)?;
        self.statement()
    }

    // <fun declaration> ::= 'fun' <identifier> ['(' (<identifier> ',')* [<identifier>] ')'] (<block> | <expression>) 
    fn fun_declaration(&mut self) -> Result<Node, ParseError> {
        self.consume(FUN)?;
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
        Ok(Node::FunctionDeclaration { name, args, body })
    }
    
    // <record declaration> ::= 'record' <identifier> '(' <identifier>* ')'
    fn record_declaration(&mut self) -> Result<Node, ParseError> {
        self.consume(RECORD)?;
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
        Ok(Node::RecordDeclaration { name, members })
    }

    // <return statement> ::= 'return' <expression>
    fn return_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(RETURN)?;
        Ok(Node::Return(Box::new(self.expression()?)))
    }

    // <let statement> ::= 'let' <identifier> '=' <expression>
    fn let_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(LET)?;
        let name = self.consume(IDENTIFIER)?.text;
        self.consume(EQUAL)?;
        let expr = Box::new(self.expression()?);
        Ok(Node::LetStatement { name, expr })  
    }

    // <import statement> ::= 'import' <string>
    fn import_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(IMPORT)?;
        Ok(Node::Import(self.consume(STRING)?.text))
    }

    // <break statement> ::= 'break'
    fn break_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(BREAK)?;
        Ok(Node::Break)
    }

    // <continue statement> ::= 'continue'
    fn continue_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(CONTINUE)?;
        Ok(Node::Continue)
    }

    // <for statement> ::= 'for' <identifier> ':' <expression> <statement>
    fn for_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(FOR)?;
        let var = self.consume(IDENTIFIER)?.text;
        self.consume(COLON)?;
        let iter = Box::new(self.expression()?);
        let body = Box::new(self.statement()?);
        Ok(Node::ForStatement { var, iter, body })
    }

    // <block> ::= '{' <statement>* '}' 
    fn block(&mut self) -> Result<Node, ParseError> {
        self.consume(LCURLY)?;
        let mut statements: Vec<Node> = vec![];
        while !self.expect(RCURLY) {
            statements.push(self.declaration()?);
        }
        Ok(Node::Block(statements))
    }

    // <module declaration> ::= 'module' <identifier> <block> 
    fn module_declaration(&mut self) -> Result<Node, ParseError> {
        self.consume(MODULE)?;
        let name = self.consume(IDENTIFIER)?.text;
        let body;
        if let Node::Block(s) = self.block()? {
            body = Box::new(Node::Module(s));
        } else {unreachable!()}
        Ok(Node::ModuleDeclaration { name, body })
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
        let mut statements = vec![];
        while !self.expect(END) {
            statements.push(self.declaration()?);
        }
        Ok(Node::Module(statements))
    }   

    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Node, ParseError> {
        self.tokens = tokens;
        self.current = 0;
        self.module()
    }

}