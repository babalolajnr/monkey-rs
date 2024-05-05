use std::any::Any;

use lexer::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
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
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
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

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &self.token.as_ref().unwrap().literal
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
