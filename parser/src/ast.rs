use std::any::Any;

use lexer::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
    fn string(&self) -> String;
}

pub trait Statement: Node + Any {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn token_literal(&self) -> &str {
        if self.statements.is_empty() {
            return "";
        }

        self.statements[0].token_literal()
    }

    pub fn string(&self) -> String {
        let mut s = String::new();

        for statement in &self.statements {
            s.push_str(&statement.string());
        }

        s
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn string(&self) -> String {
        self.value.to_owned()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

pub struct LetStatement {
    pub token: Option<Token>,
    pub name: Option<Identifier>,
    pub value: Option<Box<dyn Expression>>,
}

/// Implements the `Node` trait for the `LetStatement` struct.
impl Node for LetStatement {
    /// Returns the token literal of the `LetStatement`.
    fn token_literal(&self) -> &str {
        &self.token.as_ref().unwrap().literal
    }

    /// Returns a string representation of the `LetStatement`.
    fn string(&self) -> String {
        let mut s = String::new();

        s.push_str(self.token_literal());
        s.push(' ');
        s.push_str(&self.name.as_ref().unwrap().string());
        s.push_str(" = ");

        if let Some(value) = &self.value {
            s.push_str(value.token_literal());
        }

        s.push(';');

        s
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct ReturnStatement {
    pub token: Option<Token>,
    pub return_value: Option<Box<dyn Expression>>,
}

/// Implements the `Node` trait for the `ReturnStatement` struct.
impl Node for ReturnStatement {
    /// Returns the literal value of the token associated with the `ReturnStatement`.
    fn token_literal(&self) -> &str {
        &self.token.as_ref().unwrap().literal
    }

    /// Returns a string representation of the `ReturnStatement`.
    fn string(&self) -> String {
        let mut s = String::new();

        s.push_str(self.token_literal());
        s.push(' ');

        if let Some(value) = &self.return_value {
            s.push_str(value.token_literal());
        }

        s.push(';');

        s
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use lexer::token::TokenType;

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Box::new(LetStatement {
                    token: Some(Token {
                        token_type: TokenType::LET,
                        literal: "let".to_string(),
                    }),
                    name: Some(Identifier {
                        token: Token {
                            token_type: TokenType::IDENT,
                            literal: "myVar".to_string(),
                        },
                        value: "myVar".to_string(),
                    }),
                    value: None,
                }),
                Box::new(ReturnStatement {
                    token: Some(Token {
                        token_type: TokenType::RETURN,
                        literal: "return".to_string(),
                    }),
                    return_value: Some(Box::new(Identifier {
                        token: Token {
                            token_type: TokenType::IDENT,
                            literal: "anotherVar".to_string(),
                        },
                        value: "anotherVar".to_string(),
                    })),
                }),
            ],
        };

        assert_eq!(program.string(), "let myVar = ;return anotherVar;");
    }
}
