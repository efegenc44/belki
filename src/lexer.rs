use std::fmt::Display;

use crate::token::{ TokenKind, Token, Location };

use TokenKind::*;

#[derive(Debug)]
pub enum LexError {
    UnexpectedCharacter(Location),
    UnknownCharacter(Location),
    UnknownEscapeSequence(Location)
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedCharacter(loc)   => write!(f, "\n  An error occured while lexing:\n      Unexpected Character at {}\n", loc),
            Self::UnknownCharacter(loc)      => write!(f, "\n  An error occured while lexing:\n      Unknown Character at {}\n", loc),
            Self::UnknownEscapeSequence(loc) => write!(f, "\n  An error occured while lexing:\n      Unknown Escape Sequence at {}\n", loc),
        }
    }
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
            row: 1, col: 1
        }
    }

    fn peek(&self) -> char {
        self.source.as_bytes()[self.current] as char
    }

    fn next(&mut self) -> char {
        self.col += 1; 
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
        while self.peek().is_digit(10) {
            self.next();
        }
        
        if self.peek().is_alphabetic() {
            return Err(LexError::UnexpectedCharacter(self.get_loc()));
        }
        
        Ok(self.make_token(INTEGER))
    }

    fn identifier(&mut self) -> Result<Token, LexError> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.next();          
        }
        
        while self.peek() == '\'' {
            self.next();
        }

        let text = &self.source[self.start..self.current];
    
        let token_kind = match text {
            "record"  => RECORD,
            "let"     => LET,
            "fun"     => FUN,
            "true"    => TRUE,
            "false"   => FALSE,
            "nothing" => NOTHING,
            "if"      => IF,
            "else"    => ELSE,
            "then"    => THEN,
            "while"   => WHILE,
            "import"  => IMPORT,
            "return"  => RETURN,
            "for"     => FOR,
            "continue"=> CONTINUE,
            "break"   => BREAK,
            "module"  => MODULE,
            _         => IDENTIFIER
        };

        Ok(self.make_token(token_kind))
    }

    fn string(&mut self) -> Result<Token, LexError> {
        let mut value = String::new();
        while !(self.peek() == '\n' 
            || self.peek() == '\0'
            || self.peek() == '"') 
        {
            if self.expect('\\') {
                match self.peek() {
                    '\\' => value += "\\",
                    'n' => value += "\n",
                    '0' => value += "\0",
                    't' => value += "\t",
                    'r' => value += "\r",
                    '"' => value += "\"",
                    // FIX 
                    _ => return Err(LexError::UnknownEscapeSequence(self.get_loc()))
                } self.next(); continue;
            }
            value += &self.next().to_string();
        }
        // Closing '"'.
        if !self.expect('"') {
            return Err(LexError::UnexpectedCharacter(self.get_loc()));
        }

        Ok(Token::new(STRING, value, self.get_loc()))
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
            ';'  => Ok(self.make_token(SEMICOLON )),
            '+'  => Ok(self.make_token(PLUS      )),
            '*'  => Ok(self.make_token(STAR      )),
            '%'  => Ok(self.make_token(PERCENT   )),
            '^'  => Ok(self.make_token(CARET     )),
            '#'  => Ok(self.make_token(HASH     )),
            ','  => Ok(self.make_token(COMMA     )),
            '\0' => Ok(self.make_token(END       )),
            '_'  => Ok(self.make_token(UNDERSCORE)),
            ':'  => 
                if self.expect(':') { Ok(self.make_token(DCOLON)) } 
                else { Ok(self.make_token(COLON)) }
            '.'  => 
                if self.expect('.') { Ok(self.make_token(TWODOT)) } 
                else { Ok(self.make_token(DOT)) }
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
                self.row += 1;
                self.col = 1;
                Ok(self.make_token(NEWLINE))
            }
            // FIX 
            _ => Err(LexError::UnknownCharacter(Location::new(self.row, self.col - 1)))

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