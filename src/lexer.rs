use crate::token::{ TokenKind, Token, Location };
use crate::error::report;

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

    fn number(&mut self) -> Token {
        let mut token_kind = TokenKind::INTEGER;
        while self.peek().is_digit(10) {
            self.next();
        }
        
        if self.expect('.') {
            token_kind = TokenKind::FLOAT;
            while self.peek().is_digit(10) {
                self.next();
            }   
        }

        if self.peek().is_alphabetic() {
            report(self.get_loc(), "Invalid Number Literal");
        }
        
        Token::new(
            token_kind, 
            self.source[self.start..self.current].to_string(),
            self.get_loc()
        )
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.next();          
        }
        let text = &self.source[self.start..self.current];
    
        let token_kind = match text {
            "class"   => TokenKind::CLASS,
            "let"     => TokenKind::LET,
            "fun"     => TokenKind::FUN,
            "true"    => TokenKind::TRUE,
            "false"   => TokenKind::FALSE,
            "nothing" => TokenKind::NOTHING,
            // "print"   => TokenKind::PRINT,
            "if"      => TokenKind::IF,
            "else"    => TokenKind::ELSE,
            "while"   => TokenKind::WHILE,
            "import"  => TokenKind::IMPORT,
            "return"  => TokenKind::RETURN,
            _         => TokenKind::IDENTIFIER
        };

        Token::new(token_kind, text.to_string(), self.get_loc())
    }

    fn string(&mut self) -> Token {
        while !(self.peek() == '\n' 
            || self.peek() == '\0'
            || self.peek() == '"') 
        {
            self.next();
        }
        // Closing '"'.
        if !self.expect('"') {
            report(self.get_loc(), "Unterminated String");
        }

        Token::new(
            TokenKind::STRING, 
            self.source[self.start+1..self.current-1].to_string(),
            self.get_loc()
        )
    }

    fn get_text(&mut self) -> String {
        self.source[self.start..self.current].to_string()
    }

    fn get_loc(&self) -> Location {
        Location::new(self.row, self.col)
    }

    pub fn scan_token(&mut self) -> Token {
        self.row += 1; self.start = self.current;
        
        let c = self.next();

        if c.is_digit(10) {
            return self.number();
        } else if c.is_alphabetic() {
            return self.identifier();
        } else if c == '"' {
            return self.string();
        }

        match c {
            '('  => Token::new(TokenKind::LPAREN    , self.get_text(), self.get_loc()),
            ')'  => Token::new(TokenKind::RPAREN    , self.get_text(), self.get_loc()),
            '{'  => Token::new(TokenKind::LCURLY    , self.get_text(), self.get_loc()),
            '}'  => Token::new(TokenKind::RCURLY    , self.get_text(), self.get_loc()),
            '['  => Token::new(TokenKind::LSQUARE   , self.get_text(), self.get_loc()),
            ']'  => Token::new(TokenKind::RSQUARE   , self.get_text(), self.get_loc()),
            ':'  => Token::new(TokenKind::COLON     , self.get_text(), self.get_loc()),
            ';'  => Token::new(TokenKind::SEMICOLON , self.get_text(), self.get_loc()),
            '+'  => Token::new(TokenKind::PLUS      , self.get_text(), self.get_loc()),
            '*'  => Token::new(TokenKind::STAR      , self.get_text(), self.get_loc()),
            '%'  => Token::new(TokenKind::PERCENT   , self.get_text(), self.get_loc()),
            '^'  => Token::new(TokenKind::CARET     , self.get_text(), self.get_loc()),
            ','  => Token::new(TokenKind::COMMA     , self.get_text(), self.get_loc()),
            '.'  => Token::new(TokenKind::DOT       , self.get_text(), self.get_loc()),
            '\0' => Token::new(TokenKind::END       , self.get_text(), self.get_loc()),
            '_'  => Token::new(TokenKind::UNDERSCORE, self.get_text(), self.get_loc()),
            '-' => {
                if self.expect('>')
                    { Token::new(TokenKind::ARROW, self.get_text(), self.get_loc()) } 
                else 
                    { Token::new(TokenKind::MINUS, self.get_text(), self.get_loc()) }
            },
            '&' => {
                if self.expect('&')
                    { Token::new(TokenKind::DAMPERSAND, self.get_text(), self.get_loc()) } 
                else 
                    { Token::new(TokenKind::AMPERSAND, self.get_text(), self.get_loc()) }
            },
            '|' => 
                if self.expect('|')
                    { Token::new(TokenKind::DVLINE, self.get_text(), self.get_loc()) } 
                else if self.expect('>') {
                    Token::new(TokenKind::PIPE, self.get_text(), self.get_loc())
                } else { 
                    Token::new(TokenKind::VLINE, self.get_text(), self.get_loc()) 
                },
            '>' => {
                if self.expect('=')
                    { Token::new(TokenKind::GREATEREQUAL, self.get_text(), self.get_loc()) } 
                else 
                    { Token::new(TokenKind::GREATER, self.get_text(), self.get_loc()) }
            },
            '<' => {
                if self.expect('=')
                    { Token::new(TokenKind::LESSEQUAL, self.get_text(), self.get_loc()) } 
                else 
                    { Token::new(TokenKind::LESS, self.get_text(), self.get_loc()) }
            },
            '!' => {
                if self.expect('=')
                    { Token::new(TokenKind::BANGEQUAL, self.get_text(), self.get_loc()) } 
                else 
                    { Token::new(TokenKind::BANG, self.get_text(), self.get_loc()) }
            },
            '=' => if self.expect('=') { 
                Token::new(TokenKind::DEQUAL, self.get_text(), self.get_loc()) 
            } else { 
                Token::new(TokenKind::EQUAL, self.get_text(), self.get_loc()) 
            },
            '/' => if self.expect('/') {
                while !(self.peek() == '\n'  
                    || self.peek() == '\0') {
                        self.next();
                    } 
                Token::new(TokenKind::COMMENT, self.source[self.start+2..self.current].to_string(), self.get_loc())
            } else {
                Token::new(TokenKind::SLASH, self.get_text(), self.get_loc())
            },
            ' ' | '\t' | '\r' => Token::new(TokenKind::WHITESPACE, self.get_text(), self.get_loc()), 
            '\n' => {
                self.col += 1;
                self.row = 0;
                Token::new(TokenKind::NEWLINE, self.get_text(), self.get_loc())
            }
            _ => {
                report(self.get_loc(), "Unknown Character");
                Token::new(TokenKind::END, self.get_text(), self.get_loc())
            },

        }
    }

    pub fn tokens(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        
        let mut token = self.scan_token();
        while !(token.kind == TokenKind::END) {
            if token.kind != TokenKind::WHITESPACE &&
                token.kind != TokenKind::COMMENT &&
                token.kind != TokenKind::NEWLINE {
                    tokens.push(token);
                }
            token = self.scan_token();
             
        }
        tokens.push(Token::new(
            TokenKind::END,
            String::from("$"),
            self.get_loc()
        ));
        
        tokens 
    }

}