#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // Literals.
    STRING, IDENTIFIER, INTEGER, 

    // One character tokens.
    LPAREN, RPAREN, LCURLY, RCURLY,
    LSQUARE, RSQUARE, COLON, SEMICOLON,
    PLUS, STAR, SLASH, PERCENT, CARET,
    COMMA, DOT, UNDERSCORE, MINUS, 
    AMPERSAND, VLINE, GREATER, LESS,
    BANG, EQUAL, HASH,
    
    // Double character tokens.
    DAMPERSAND, DVLINE, DEQUAL, BANGEQUAL,
    GREATEREQUAL, LESSEQUAL, ARROW, PIPE,
    TWODOT, DCOLON,

    // Keywords.
    RECORD, LET, FUN, TRUE, FALSE, 
    NOTHING, IF, ELSE, FOR, BREAK, CONTINUE,
    WHILE, IMPORT, RETURN, MODULE, THEN, 

    // WS Tokens.
    END, COMMENT, WHITESPACE, NEWLINE
}

#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub file: String,
    pub row: usize,
    pub col: usize,
}

impl Location {
    pub fn new(row: usize, col: usize, file: String) -> Location {
        Location { row, col, file }
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.row, self.col)
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