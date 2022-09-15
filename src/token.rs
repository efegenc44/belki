#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // Literals.
    STRING, IDENTIFIER, INTEGER, FLOAT, 

    // One character tokens.
    LPAREN, RPAREN, LCURLY, RCURLY,
    LSQUARE, RSQUARE, COLON, SEMICOLON,
    PLUS, STAR, SLASH, PERCENT, CARET,
    COMMA, DOT, UNDERSCORE, MINUS, 
    AMPERSAND, VLINE, GREATER, LESS,
    BANG, EQUAL,
    
    // Double character tokens.
    DAMPERSAND, DVLINE, DEQUAL, BANGEQUAL,
    GREATEREQUAL, LESSEQUAL, ARROW, PIPE,

    // Keywords.
    CLASS, LET, FUN, TRUE, FALSE, 
    NOTHING, PRINT, IF, ELSE, 
    WHILE, IMPORT, RETURN,   

    // WS Tokens.
    END, COMMENT, WHITESPACE, NEWLINE
}

#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

impl Location {
    pub fn new(row: usize, col: usize) -> Location {
        Location { row, col }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: Location 
}

impl Token {
    pub fn new(kind: TokenKind, text: String, loc: Location ) -> Token {
        Token { kind, text, loc }
    }
}