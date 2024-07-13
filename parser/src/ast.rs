use std::{any::Any, fmt::Debug};

use lexer::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
    fn string(&self) -> String;
}

pub trait Statement: Node + Any + Debug {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node + Any + Debug {
    fn expression_node(&self);
    fn as_any(&self) -> &dyn Any;
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

#[derive(Debug)]
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

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Option<Token>,
    pub expression: Option<Box<dyn Expression>>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &self.token.as_ref().unwrap().literal
    }

    fn string(&self) -> String {
        if let Some(expression) = &self.expression {
            expression.string()
        } else {
            String::new()
        }
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn string(&self) -> String {
        self.token.literal.to_owned()
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn string(&self) -> String {
        let mut s = String::new();

        s.push('(');
        s.push_str(&self.operator);
        s.push_str(&self.right.string());
        s.push(')');

        s
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

// Resume here
#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn string(&self) -> String {
        let mut s = String::new();

        s.push('(');
        s.push_str(&self.left.string());
        s.push(' ');
        s.push_str(&self.operator);
        s.push(' ');
        s.push_str(&self.right.string());
        s.push(')');

        s
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
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
