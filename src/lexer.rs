use crate::token::{ TokenKind, Token, Location };

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
            "class"   => TokenKind::CLASS,
            "let"     => TokenKind::LET,
            "fun"     => TokenKind::FUN,
            "true"    => TokenKind::TRUE,
            "false"   => TokenKind::FALSE,
            "nothing" => TokenKind::NOTHING,
            "if"      => TokenKind::IF,
            "else"    => TokenKind::ELSE,
            "while"   => TokenKind::WHILE,
            "import"  => TokenKind::IMPORT,
            "return"  => TokenKind::RETURN,
            _         => TokenKind::IDENTIFIER
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
            TokenKind::STRING, 
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
            '('  => Ok(self.make_token(TokenKind::LPAREN    )),
            ')'  => Ok(self.make_token(TokenKind::RPAREN    )),
            '{'  => Ok(self.make_token(TokenKind::LCURLY    )),
            '}'  => Ok(self.make_token(TokenKind::RCURLY    )),
            '['  => Ok(self.make_token(TokenKind::LSQUARE   )),
            ']'  => Ok(self.make_token(TokenKind::RSQUARE   )),
            ':'  => Ok(self.make_token(TokenKind::COLON     )),
            ';'  => Ok(self.make_token(TokenKind::SEMICOLON )),
            '+'  => Ok(self.make_token(TokenKind::PLUS      )),
            '*'  => Ok(self.make_token(TokenKind::STAR      )),
            '%'  => Ok(self.make_token(TokenKind::PERCENT   )),
            '^'  => Ok(self.make_token(TokenKind::CARET     )),
            ','  => Ok(self.make_token(TokenKind::COMMA     )),
            '.'  => Ok(self.make_token(TokenKind::DOT       )),
            '\0' => Ok(self.make_token(TokenKind::END       )),
            '_'  => Ok(self.make_token(TokenKind::UNDERSCORE)),
            '-' => 
                if self.expect('>') { Ok(self.make_token(TokenKind::ARROW)) } 
                else { Ok(self.make_token(TokenKind::MINUS)) }
            '&' =>
                if self.expect('&') { Ok(self.make_token(TokenKind::DAMPERSAND)) } 
                else { Ok(self.make_token(TokenKind::AMPERSAND)) }
            '|' => 
                if self.expect('|') { Ok(self.make_token(TokenKind::DVLINE)) } 
                else if self.expect('>') { Ok(self.make_token(TokenKind::PIPE)) } 
                else { Ok(self.make_token(TokenKind::VLINE)) },
            '>' => 
                if self.expect('=') { Ok(self.make_token(TokenKind::GREATEREQUAL)) } 
                else { Ok(self.make_token(TokenKind::GREATER)) }
            '<' => 
                if self.expect('=') { Ok(self.make_token(TokenKind::LESSEQUAL)) } 
                else { Ok(self.make_token(TokenKind::LESS)) }
            '!' => 
                if self.expect('=') { Ok(self.make_token(TokenKind::BANGEQUAL)) } 
                else { Ok(self.make_token(TokenKind::BANG)) }
            '=' => 
                if self.expect('=') { Ok(self.make_token(TokenKind::DEQUAL)) } 
                else { Ok(self.make_token(TokenKind::EQUAL)) }
            '/' => 
                if self.expect('/') {
                    while !(self.peek() == '\n'  
                        || self.peek() == '\0') {
                            self.next();
                    } 
                    Ok(Token::new(TokenKind::COMMENT, self.source[self.start+2..self.current].to_string(), self.get_loc()))
                } else { Ok(self.make_token(TokenKind::SLASH)) }
            ' ' | '\t' | '\r' => Ok(self.make_token(TokenKind::WHITESPACE)), 
            '\n' => {
                self.col += 1;
                self.row = 0;
                Ok(self.make_token(TokenKind::NEWLINE))
            }
            _ => {
                Err(LexError::UnknownCharacter)
            },

        }
    }

    pub fn tokens(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens: Vec<Token> = vec![];
        
        let mut token = self.scan_token()?;
        while !(token.kind == TokenKind::END) {
            if token.kind != TokenKind::WHITESPACE &&
                token.kind != TokenKind::COMMENT &&
                token.kind != TokenKind::NEWLINE {
                    tokens.push(token);
                }
            token = self.scan_token()?;
        }
        tokens.push(Token::new(
            TokenKind::END,
            String::from("$"),
            self.get_loc()
        ));
        
        Ok(tokens) 
    }

}