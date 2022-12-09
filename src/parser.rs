use crate::token::{ Token, TokenKind, Location };
use crate::ast::Node;

use TokenKind::*;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken,
    ReturnOutsideFunction,
    IllDefinedAST
}

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
//                       | <product> ?no newline? '(' (<expression>',')* [<expression>] ')'
//                       | <product> ?no newline? '[' <expression> ']' 
//                       | '[' (<expression> ',')* [<expression>] ']' 




pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    inside_function: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0, inside_function: 0 }
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

    #[allow(dead_code)]
    fn get_loc(&mut self) -> Location {
        self.peek().loc
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
            dbg!(self.peek());
            dbg!(expected);
            Err(ParseError::UnexpectedToken)
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
                    return Ok(Node::Float(text.parse::<f32>().unwrap()))
                }
                Node::Integer(t.text.parse::<i32>().unwrap())
            },
            STRING     => Node::String(t.text.to_string()),
            IDENTIFIER => Node::Identifier(t.text),
            TRUE       => Node::True,
            FALSE      => Node::False,
            NOTHING    => Node::Nothing,
            LSQUARE    => Node::List(self.expression_list(RSQUARE)?),
            LPAREN     => {
                let expr = self.expression()?;
                self.consume(RPAREN)?;
                Node::Group(Box::new(expr))
            },
            PLUS | MINUS | BANG => {
                let operand = self.product()?;
                Node::Unary {
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
                let body = Box::new(self.fun_block()?);
                Node::Fun { name: "".into(), args, body }
            }
            
            _ => {
                dbg!(t);
                return Err(ParseError::IllDefinedAST);
            }
        };
        
        // may written better
        loop {
            let ntnl = self.peek_with_nl();
            match ntnl.kind {
                LSQUARE => {
                    self.consume(LSQUARE)?;
                    let rhs = self.expression()?;
                    self.consume(RSQUARE)?;
                    node = Node::Binary { 
                        op: ntnl.text, 
                        lhs: Box::new(node),
                        rhs: Box::new(rhs), 
                    }; continue;
                },
                LPAREN => {
                    self.consume(LPAREN)?;
                    let args = self.expression_list(RPAREN)?;
                    node = Node::FunCall { 
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

                    node = Node::Binary { 
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

    fn term(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.product()?;
        while let STAR | SLASH | PERCENT = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.product()?;
            lhs = Node::Binary {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs)
            };
        } 
        Ok(lhs)
    }

    pub fn arithmetic(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.term()?;
        while let PLUS | MINUS | PIPE = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.term()?;
            lhs = Node::Binary {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs)
            };
        }
        Ok(lhs)
    }

    fn relation(&mut self) -> Result<Node, ParseError> {
        let lhs = self.arithmetic()?;
        if let DEQUAL | BANGEQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.arithmetic()?;
            Ok(Node::Binary { 
                op: t.text, rhs: Box::new(rhs), lhs: Box::new(lhs) 
            })
        } else { Ok(lhs) }
    }

    pub fn logic(&mut self) -> Result<Node, ParseError> {
        let mut lhs = self.relation()?;
        while let DVLINE | DAMPERSAND = self.peek().kind {
            let t = self.peek().clone();
            self.consume(t.kind)?;
            let rhs = self.relation()?;
            lhs = Node::Binary {
                op: t.text, lhs: Box::new(lhs), rhs: Box::new(rhs)
            };
        }
        Ok(lhs)
    }

    pub fn expression(&mut self) -> Result<Node, ParseError> {
        let lhs = self.logic()?;
        match self.peek().kind {
            EQUAL => {
                self.consume(EQUAL)?;
                let rhs = self.expression()?;
                Ok(Node::Binary { op: "=".into(), lhs: Box::new(lhs), rhs: Box::new(rhs) })
            },
            TWODOT => {
                self.consume(TWODOT).unwrap();
                let rhs = self.expression()?;
                Ok(Node::Binary { 
                    op: "..".into(), lhs: Box::new(lhs), rhs: Box::new(rhs) 
                })
            }
            _ => Ok(lhs)
        }
    }

    fn while_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(WHILE)?;
        Ok(Node::While { expr: Box::new(self.expression()?), body: Box::new(self.block()?) })
    }

    fn if_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(IF)?;
        Ok(Node::If { 
            expr: Box::new(self.expression()?), 
            body: Box::new(self.block()?), 
            els: if self.peek().kind == ELSE { Box::new(self.els()?) } else { Box::new(Node::None) } 
        })
    }

    fn els(&mut self) -> Result<Node, ParseError> {
        self.consume(ELSE)?;
        if self.peek().kind == LCURLY {
            Ok(Node::Else { expr: Box::new(Node::None), body: Box::new(self.block()?), els: Box::new(Node::None) })
        } else {
            Ok(Node::Else { 
                expr: Box::new(self.expression()?), 
                body: Box::new(self.block()?), 
                els: if self.peek().kind == ELSE { Box::new(self.els()?) } else { Box::new(Node::None) } 
            })
        }
    }

    // todo
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
        let body = Box::new(self.fun_block()?);
        Ok(Node::Fun { name, args, body })
    }
    
    // todo
    fn class_declaration(&mut self) -> Result<Node, ParseError> {
        self.consume(CLASS)?;
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
        Ok(Node::Class { name, members })
    }

    fn return_statement(&mut self) -> Result<Node, ParseError> {
        if self.inside_function == 0 {
            return Err(ParseError::ReturnOutsideFunction);
        }
        self.consume(RETURN)?;
        Ok(Node::Return(Box::new(self.expression()?)))
    }

    fn let_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(LET)?;
        let name = self.consume(IDENTIFIER)?.text;
        self.consume(EQUAL)?;
        let expr = Box::new(self.expression()?);
        Ok(Node::Let { name, expr })  
    }

    fn import_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(IMPORT)?;
        Ok(Node::Import(self.consume(STRING)?.text))
    }

    fn break_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(BREAK)?;
        Ok(Node::Break)
    }

    fn continue_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(CONTINUE)?;
        Ok(Node::Continue)
    }

    fn for_statement(&mut self) -> Result<Node, ParseError> {
        self.consume(FOR)?;
        let var = self.consume(IDENTIFIER)?.text;
        self.consume(COLON)?;
        let iter = Box::new(self.expression()?);
        let body = Box::new(self.block()?);
        Ok(Node::For { var, iter, body })
    }

    fn block(&mut self) -> Result<Node, ParseError> {
        self.consume(LCURLY)?;
        let mut statements: Vec<Node> = vec![];
        while !self.expect(RCURLY) {
            statements.push(self.statement()?);
        }
        Ok(Node::Block(statements))
    }

    fn block2(&mut self) -> Result<Node, ParseError> {
        self.consume(LCURLY)?;
        let mut statements: Vec<Node> = vec![];
        while !self.expect(RCURLY) {
            statements.push(self.statement()?);
        }
        Ok(Node::Block2(statements))
    }

    fn fun_block(&mut self) -> Result<Node, ParseError> {
        self.inside_function += 1;
        self.consume(LCURLY)?;
        let mut statements: Vec<Node> = vec![];
        while !self.expect(RCURLY) {
            statements.push(self.statement()?);
        }
        self.inside_function -= 1;
        Ok(Node::FunBlock(statements))
    }

    fn module(&mut self) -> Result<Node, ParseError> {
        self.consume(MODULE)?;
        let name = self.consume(IDENTIFIER)?.text;
        let block = Box::new(self.block2()?);
        Ok(Node::Module(name, block))
    }

    fn statement(&mut self) -> Result<Node, ParseError> {
        let node = match self.peek().kind {
            LCURLY   => self.block()?,
            LET      => self.let_statement()?,
            RETURN   => self.return_statement()?,
            BREAK    => self.break_statement()?,
            CONTINUE => self.continue_statement()?,
            CLASS    => self.class_declaration()?,
            FUN      => self.fun_declaration()?,
            IF       => self.if_statement()?,
            WHILE    => self.while_statement()?,
            IMPORT   => self.import_statement()?,
            FOR      => self.for_statement()?,
            MODULE   => self.module()?,
            _        => self.expression()?,
        };
        // Optional semi-colon
        self.expect(SEMICOLON);
        Ok(node)
    }

    fn program(&mut self) -> Result<Node, ParseError> {
        let mut statements = vec![];
        while !self.expect(END) {
            statements.push(self.statement()?);
        }
        Ok(Node::Program(statements))
    }   

    pub fn parse(&mut self) -> Result<Node, ParseError> {
        self.program()
    }

}
