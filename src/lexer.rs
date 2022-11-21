use crate::token::{ TokenKind, Token, Location };

use TokenKind::*;

#[derive(Debug)]
pub enum LexError {
    UnexpectedCharacter,
    UnknownCharacter,
}

pub struct Lexer {
    source: String,

    // Absolute position
    start: usize,
    current: usize,
    
    // Row and Column
    row: usize,
    col: usize
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        Lexer {
            source: source + "\0",
            start: 0, current: 0,
            row: 0, col: 1
        }
    }

    fn peek(&self) -> char {
        self.source.as_bytes()[self.current] as char
    }

    fn next(&mut self) -> char {
        self.row += 1; 
        self.current += 1;
        self.source.as_bytes()[self.current - 1] as char
    }

    fn expect(&mut self, expected: char) -> bool {
        if self.peek() == expected {
            self.next(); true
        } else {
            false
        }
    }

    fn number(&mut self) -> Result<Token, LexError> {
        let mut token_kind = INTEGER;
        while self.peek().is_digit(10) {
            self.next();
        }
        
        if self.expect('.') {
            token_kind = FLOAT;
            while self.peek().is_digit(10) {
                self.next();
            }   
        }

        if self.peek().is_alphabetic() {
            return Err(LexError::UnexpectedCharacter);
        }
        
        Ok(self.make_token(token_kind))
    }

    fn identifier(&mut self) -> Result<Token, LexError> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.next();          
        }
        let text = &self.source[self.start..self.current];
    
        let token_kind = match text {
            "class"   => CLASS,
            "let"     => LET,
            "fun"     => FUN,
            "true"    => TRUE,
            "false"   => FALSE,
            "nothing" => NOTHING,
            "if"      => IF,
            "else"    => ELSE,
            "while"   => WHILE,
            "import"  => IMPORT,
            "return"  => RETURN,
            _         => IDENTIFIER
        };

        Ok(self.make_token(token_kind))
    }

    fn string(&mut self) -> Result<Token, LexError> {
        while !(self.peek() == '\n' 
            || self.peek() == '\0'
            || self.peek() == '"') 
        {
            self.next();
        }
        // Closing '"'.
        if !self.expect('"') {
            return Err(LexError::UnexpectedCharacter);
        }

        Ok(Token::new(
            STRING, 
            self.source[self.start+1..self.current-1].to_string(),
            self.get_loc()
        ))
    }

    fn get_text(&mut self) -> String {
        self.source[self.start..self.current].to_string()
    }

    fn get_loc(&self) -> Location {
        Location::new(self.row, self.col)
    }

    fn make_token(&mut self, kind: TokenKind) -> Token {
        Token::new(kind, self.get_text(), self.get_loc())
    }

    pub fn scan_token(&mut self) -> Result<Token, LexError> {
        self.start = self.current;
        
        let c = self.next();

        if c.is_digit(10) {
            return self.number();
        } else if c.is_alphabetic() {
            return self.identifier();
        } else if c == '"' {
            return self.string();
        }

        match c {
            '('  => Ok(self.make_token(LPAREN    )),
            ')'  => Ok(self.make_token(RPAREN    )),
            '{'  => Ok(self.make_token(LCURLY    )),
            '}'  => Ok(self.make_token(RCURLY    )),
            '['  => Ok(self.make_token(LSQUARE   )),
            ']'  => Ok(self.make_token(RSQUARE   )),
            ':'  => Ok(self.make_token(COLON     )),
            ';'  => Ok(self.make_token(SEMICOLON )),
            '+'  => Ok(self.make_token(PLUS      )),
            '*'  => Ok(self.make_token(STAR      )),
            '%'  => Ok(self.make_token(PERCENT   )),
            '^'  => Ok(self.make_token(CARET     )),
            ','  => Ok(self.make_token(COMMA     )),
            '.'  => Ok(self.make_token(DOT       )),
            '\0' => Ok(self.make_token(END       )),
            '_'  => Ok(self.make_token(UNDERSCORE)),
            '-' => 
                if self.expect('>') { Ok(self.make_token(ARROW)) } 
                else { Ok(self.make_token(MINUS)) }
            '&' =>
                if self.expect('&') { Ok(self.make_token(DAMPERSAND)) } 
                else { Ok(self.make_token(AMPERSAND)) }
            '|' => 
                if self.expect('|') { Ok(self.make_token(DVLINE)) } 
                else if self.expect('>') { Ok(self.make_token(PIPE)) } 
                else { Ok(self.make_token(VLINE)) },
            '>' => 
                if self.expect('=') { Ok(self.make_token(GREATEREQUAL)) } 
                else { Ok(self.make_token(GREATER)) }
            '<' => 
                if self.expect('=') { Ok(self.make_token(LESSEQUAL)) } 
                else { Ok(self.make_token(LESS)) }
            '!' => 
                if self.expect('=') { Ok(self.make_token(BANGEQUAL)) } 
                else { Ok(self.make_token(BANG)) }
            '=' => 
                if self.expect('=') { Ok(self.make_token(DEQUAL)) } 
                else { Ok(self.make_token(EQUAL)) }
            '/' => 
                if self.expect('/') {
                    while !(self.peek() == '\n'  
                        || self.peek() == '\0') {
                            self.next();
                    } 
                    Ok(Token::new(COMMENT, self.source[self.start+2..self.current].to_string(), self.get_loc()))
                } else { Ok(self.make_token(SLASH)) }
            ' ' | '\t' | '\r' => Ok(self.make_token(WHITESPACE)), 
            '\n' => {
                self.col += 1;
                self.row = 0;
                Ok(self.make_token(NEWLINE))
            }
            _ => {
                Err(LexError::UnknownCharacter)
            },

        }
    }

    pub fn tokens(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens: Vec<Token> = vec![];
        
        let mut token = self.scan_token()?;
        while !(token.kind == END) {
            if token.kind != WHITESPACE &&
                token.kind != COMMENT {
                    tokens.push(token);
                }
            token = self.scan_token()?;
        }
        tokens.push(Token::new(
            END, String::from("$"), self.get_loc()
        ));
        Ok(tokens) 
    }

}