use crate::token::{ TokenKind, Token, Location };

use TokenKind::*;

type LexError = String;

fn lex_error(msg: String, loc: Location) -> LexError {
    format!("\n  An error occured while lexing:\n    {}\n    {}\n", loc, msg)  
}

pub struct Lexer {
    file: String,
    source: Vec<char>,

    // Absolute position
    start: usize,
    current: usize,
    
    // Row and Column
    row: usize,
    col: usize
}

impl Lexer {
    pub fn new(file: String) -> Lexer {
        Lexer {
            file,
            source: vec![],
            start: 0, current: 0,
            row: 1, col: 1
        }
    }

    fn peek(&self) -> char {
        self.source[self.current]
    }

    fn next(&mut self) -> char {
        self.col += 1; 
        self.current += 1;
        self.source[self.current - 1]
    }

    fn expect(&mut self, expected: char) -> bool {
        if self.peek() == expected {
            self.next(); true
        } else {
            false
        }
    } 

    fn number(&mut self) -> Result<Token, LexError> {
        let loc = self.get_loc();
        while self.peek().is_digit(10) {
            self.next();
        }
        
        if self.peek().is_alphabetic() {
            return Err(lex_error(
                format!("Unexpected Character '{}' at Number Sequence", self.peek()),
                self.get_loc()
            ));
        }
        
        Ok(self.make_token(INTEGER, loc))
    }

    fn identifier(&mut self) -> Result<Token, LexError> {
        let loc = self.get_loc();
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.next();          
        }
        
        while self.peek() == '\'' {
            self.next();
        }
        
        let text = &self.source[self.start..self.current].iter().collect::<String>();
    
        let token_kind = match text.as_str() {
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

        Ok(self.make_token(token_kind, loc))
    }

    fn string(&mut self) -> Result<Token, LexError> {
        let loc = self.get_loc();
        self.expect('"');
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
                    _ => return Err(lex_error(
                        format!("Unexpected Escape Sequence '\\{}'", self.peek()),
                        self.get_loc()
                    ))
                } self.next(); continue;
            }
            value += &self.next().to_string();
        }
        // Closing '"'.
        if !self.expect('"') {
            return Err(lex_error(
                format!("Unterminated String"),
                self.get_loc()
            ));
        }

        Ok(Token::new(STRING, value, loc))
    }

    fn get_text(&mut self) -> String {
        self.source[self.start..self.current].iter().collect()
    }

    fn get_loc(&self) -> Location {
        Location::new(self.row, self.col, self.file.clone())
    }

    fn make_token(&mut self, kind: TokenKind, loc: Location) -> Token {
        Token::new(kind, self.get_text(), loc)
    }

    pub fn scan_token(&mut self) -> Result<Token, LexError> {
        self.start = self.current;
        let loc = self.get_loc();

        let c = self.peek(); 
        if c.is_digit(10) {
            return self.number();
        } else if c.is_alphabetic() {
            return self.identifier();
        } else if c == '"' {
            return self.string();
        }
        
        match self.next() {
            '('  => Ok(self.make_token(LPAREN    , loc)),
            ')'  => Ok(self.make_token(RPAREN    , loc)),
            '{'  => Ok(self.make_token(LCURLY    , loc)),
            '}'  => Ok(self.make_token(RCURLY    , loc)),
            '['  => Ok(self.make_token(LSQUARE   , loc)),
            ']'  => Ok(self.make_token(RSQUARE   , loc)),
            ';'  => Ok(self.make_token(SEMICOLON , loc)),
            '+'  => Ok(self.make_token(PLUS      , loc)),
            '*'  => Ok(self.make_token(STAR      , loc)),
            '%'  => Ok(self.make_token(PERCENT   , loc)),
            '^'  => Ok(self.make_token(CARET     , loc)),
            '#'  => Ok(self.make_token(HASH      , loc)),
            ','  => Ok(self.make_token(COMMA     , loc)),
            '\0' => Ok(self.make_token(END       , loc)),
            '_'  => Ok(self.make_token(UNDERSCORE, loc)),
            ':'  => 
                if self.expect(':') { Ok(self.make_token(DCOLON, loc)) } 
                else { Ok(self.make_token(COLON, loc)) }
            '.'  => 
                if self.expect('.') { Ok(self.make_token(TWODOT, loc)) } 
                else { Ok(self.make_token(DOT, loc)) }
            '-' => 
                if self.expect('>') { Ok(self.make_token(ARROW, loc)) } 
                else { Ok(self.make_token(MINUS, loc)) }
            '&' =>
                if self.expect('&') { Ok(self.make_token(DAMPERSAND, loc)) } 
                else { Ok(self.make_token(AMPERSAND, loc)) }
            '|' => 
                if self.expect('|') { Ok(self.make_token(DVLINE, loc)) } 
                else if self.expect('>') { Ok(self.make_token(PIPE, loc)) } 
                else { Ok(self.make_token(VLINE, loc)) },
            '>' => 
                if self.expect('=') { Ok(self.make_token(GREATEREQUAL, loc)) } 
                else { Ok(self.make_token(GREATER, loc)) }
            '<' => 
                if self.expect('=') { Ok(self.make_token(LESSEQUAL, loc)) } 
                else { Ok(self.make_token(LESS, loc)) }
            '!' => 
                if self.expect('=') { Ok(self.make_token(BANGEQUAL, loc)) } 
                else { Ok(self.make_token(BANG, loc)) }
            '=' => 
                if self.expect('=') { Ok(self.make_token(DEQUAL, loc)) } 
                else { Ok(self.make_token(EQUAL, loc)) }
            '/' => 
                if self.expect('/') {
                    while !(self.peek() == '\n'  
                        || self.peek() == '\0') {
                            self.next();
                    } 
                    Ok(Token::new(COMMENT, self.source[self.start+2..self.current].iter().collect(), loc))
                } else { Ok(self.make_token(SLASH, loc)) }
            ' ' | '\t' | '\r' => Ok(self.make_token(WHITESPACE, loc)), 
            '\n' => {
                self.row += 1;
                self.col = 1;
                Ok(self.make_token(NEWLINE, loc))
            }
            // FIX 
            _ => Err(lex_error(
                format!("Unknown Character '{}'", c),
                self.get_loc()
            ))

        }
    }

    pub fn tokens(&mut self, source: String) -> Result<Vec<Token>, LexError> {
        self.source = (source + "\0").chars().collect::<Vec<_>>();
        self.start = 0; 
        self.current = 0;
        self.row = 1; 
        self.col = 1;     
        
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