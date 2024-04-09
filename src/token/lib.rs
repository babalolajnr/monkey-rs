/// Represents the different types of tokens in the Monkey programming language.
#[derive(PartialEq, Debug)]
pub enum TokenType {
    ILLEGAL, // Represents an illegal or unrecognized token
    EOF,     // Represents the end of the file

    // Identifiers + literals
    IDENT, // Represents an identifier
    INT,   // Represents an integer literal

    // Operators
    ASSIGN,   // Represents the assignment operator
    PLUS,     // Represents the addition operator
    MINUS,    // Represents the subtraction operator
    BANG,     // Represents the negation operator
    ASTERISK, // Represents the multiplication operator
    SLASH,    // Represents the division operator
    LT,       // Represents the less than operator
    GT,       // Represents the greater than operator

    // Delimiters
    COMMA,     // Represents a comma
    SEMICOLON, // Represents a semicolon
    LPAREN,    // Represents a left parenthesis
    RPAREN,    // Represents a right parenthesis
    LBRACE,    // Represents a left brace
    RBRACE,    // Represents a right brace

    // Keywords
    FUNCTION, // Represents the "fn" keyword
    LET,      // Represents the "let" keyword
    TRUE,     // Represents the "true" keyword
    FALSE,    // Represents the "false" keyword
    IF,       // Represents the "if" keyword
    ELSE,     // Represents the "else" keyword
    RETURN,   // Represents the "return" keyword
    EQ,       // Represents the equality operator "=="
    NOTEQ,    // Represents the inequality operator "!="
}

/// Represents a token in the Monkey programming language.
#[derive(PartialEq, Debug)]
pub struct Token {
    pub token_type: TokenType, // The type of the token
    pub literal: String,       // The literal value of the token
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }
}

impl TokenType {
    pub const ILLEGAL: &'static str = "ILLEGAL";
    pub const EOF: &'static str = "EOF";

    // Identifiers + literals
    pub const IDENT: &'static str = "IDENT";
    pub const INT: &'static str = "INT";

    // Operators
    pub const ASSIGN: &'static str = "=";
    pub const PLUS: &'static str = "+";
    pub const MINUS: &'static str = "-";
    pub const BANG: &'static str = "!";
    pub const ASTERISK: &'static str = "*";
    pub const SLASH: &'static str = "/";
    pub const LT: &'static str = "<";
    pub const GT: &'static str = ">";

    // Delimiters
    pub const COMMA: &'static str = ",";
    pub const SEMICOLON: &'static str = ";";
    pub const LPAREN: &'static str = "(";
    pub const RPAREN: &'static str = ")";
    pub const LBRACE: &'static str = "{";
    pub const RBRACE: &'static str = "}";

    // Keywords
    pub const FUNCTION: &'static str = "FUNCTION";
    pub const LET: &'static str = "LET";
    pub const TRUE: &'static str = "TRUE";
    pub const FALSE: &'static str = "FALSE";
    pub const IF: &'static str = "IF";
    pub const ELSE: &'static str = "ELSE";
    pub const RETURN: &'static str = "RETURN";
    pub const EQ: &'static str = "==";
    pub const NOT_EQ: &'static str = "!=";
}

/// Looks up the TokenType for a given identifier.
///
/// # Arguments
///
/// * `ident` - The identifier to look up.
///
/// # Returns
///
/// The TokenType corresponding to the identifier. If the identifier is not a keyword,
/// TokenType::IDENT is returned.
pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::FUNCTION,
        "let" => TokenType::LET,
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "return" => TokenType::RETURN,
        _ => TokenType::IDENT,
    }
}
