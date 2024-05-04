/// Represents the different types of tokens in the Monkey programming language.
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenType {
    ILLEGAL, // Represents an illegal or unrecognized token
    EOF,     // Represents the end of the file

    // Identifiers + literals
    /// Represents an identifier
    IDENT,

    /// Represents an integer literal
    INT,

    // Operators
    /// Represents the assignment operator
    ASSIGN,

    /// Represents the addition operator
    PLUS,

    /// Represents the subtraction operator
    MINUS,

    /// Represents the negation operator
    BANG,

    /// Represents the multiplication operator
    ASTERISK,

    /// Represents the division operator
    SLASH,

    /// Represents the less than operator
    LT,

    /// Represents the greater than operator
    GT,

    // Delimiters
    /// Represents a comma
    COMMA,

    /// Represents a semicolon
    SEMICOLON,

    /// Represents a left parenthesis
    LPAREN,

    /// Represents a right parenthesis
    RPAREN,

    /// Represents a left brace
    LBRACE,

    /// Represents a right brace
    RBRACE,

    // Keywords
    /// Represents the "fn" keyword
    FUNCTION,

    /// Represents the "let" keyword
    LET,

    /// Represents the "true" keyword
    TRUE,

    /// Represents the "false" keyword
    FALSE,

    /// Represents the "if" keyword
    IF,

    /// Represents the "else" keyword
    ELSE,

    /// Represents the "return" keyword
    RETURN,

    /// Represents the equality operator "=="
    EQ,

    /// Represents the inequality operator "!="
    NOTEQ,
}

/// Represents a token in the Monkey programming language.
#[derive(PartialEq, Debug, Clone)]
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
