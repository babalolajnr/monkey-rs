/// The `parser` module contains the implementation of the Monkey programming language parser.
pub mod ast;

use anyhow::{anyhow, Result};
use lexer::{
    token::{Token, TokenType},
    Lexer,
};

/// The `Parser` struct represents the Monkey parser.
#[derive(Debug, Clone)]
pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

impl Parser {
    /// Creates a new Parser instance with the given Lexer.
    ///
    /// # Arguments
    ///
    /// * `lexer` - The Lexer used to tokenize the input source code.
    ///
    /// # Returns
    ///
    /// A new Parser instance.
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            errors: Vec::new(),
            current_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();
        parser
    }

    /// Advances to the next token in the input source code.
    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = Some(self.lexer.next_token());
    }

    /// Returns a clone of the error messages encountered during parsing.
    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    /// Reports an error for an expected token that does not match the next token.
    ///
    /// # Arguments
    ///
    /// * `token_type` - The expected token type.
    fn peek_error(&mut self, token_type: TokenType) {
        let error = format!(
            "expected next token to be {:?}, got {:?} instead",
            token_type,
            self.peek_token.as_ref().unwrap().token_type
        );
        self.errors.push(error);
    }

    /// Parses a statement and returns it as a boxed trait object.
    ///
    /// # Returns
    ///
    /// A boxed trait object representing the parsed statement.
    fn parse_statement(&mut self) -> Result<Box<dyn ast::Statement>> {
        let token_type = self
            .current_token
            .as_ref()
            .ok_or_else(|| anyhow!("No current token"))?
            .token_type;

        match token_type {
            TokenType::LET => self
                .parse_let_statement()
                .ok_or_else(|| anyhow!("Failed to parse let statement"))
                .map(box_statement),
            TokenType::RETURN => self
                .parse_return_statement()
                .ok_or_else(|| anyhow!("Failed to parse return statement"))
                .map(box_statement),
            _ => Err(anyhow!("Invalid statement type")),
        }
    }

    /// Parses the entire program and returns the resulting AST.
    ///
    /// # Returns
    ///
    /// The parsed program as an AST.
    pub fn parse_program(&mut self) -> Result<ast::Program> {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while self.current_token.as_ref().unwrap().token_type != TokenType::EOF {
            let statement = self.parse_statement()?;
            program.statements.push(statement);
            self.next_token();
        }

        Ok(program)
    }

    /// Parses a let statement and returns it as an Option.
    ///
    /// # Returns
    ///
    /// An Option containing the parsed let statement, or None if parsing fails.
    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let mut statement = ast::LetStatement {
            token: self.current_token.clone(),
            name: None,
            value: None,
        };

        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        statement.name = Some(ast::Identifier {
            token: self.current_token.clone()?,
            value: self.current_token.as_ref().unwrap().literal.clone(),
        });

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        while !self.current_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(statement)
    }

    /// Parses a return statement and returns it as an Option.
    ///
    /// # Returns
    ///
    /// An Option containing the parsed return statement, or None if parsing fails.
    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let statement = ast::ReturnStatement {
            token: self.current_token.clone(),
            return_value: None,
        };

        self.next_token();

        while !self.current_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(statement)
    }

    /// Checks if the current token matches the given token type.
    ///
    /// # Arguments
    ///
    /// * `token_type` - The token type to check against.
    ///
    /// # Returns
    ///
    /// true if the current token matches the given token type, false otherwise.
    fn current_token_is(&mut self, token_type: TokenType) -> bool {
        self.current_token.as_ref().map(|t| t.token_type) == Some(token_type)
    }

    /// Checks if the next token matches the given token type.
    ///
    /// # Arguments
    ///
    /// * `token_type` - The token type to check against.
    ///
    /// # Returns
    ///
    /// true if the next token matches the given token type, false otherwise.
    fn peek_token_is(&mut self, token_type: TokenType) -> bool {
        self.peek_token.as_ref().map(|t| t.token_type) == Some(token_type)
    }

    /// Checks if the next token matches the given token type and advances to the next token if it does.
    ///
    /// # Arguments
    ///
    /// * `token_type` - The token type to check against.
    ///
    /// # Returns
    ///
    /// true if the next token matches the given token type, false otherwise.
    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }
}

/// Boxes a statement into a trait object.
///
/// # Arguments
///
/// * `stmt` - The statement to box.
///
/// # Returns
///
/// A boxed trait object representing the statement.
fn box_statement(stmt: impl ast::Statement) -> Box<dyn ast::Statement> {
    Box::new(stmt)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let y = 10;
            let foobar = 838383;"#;

        let lexer = Lexer::new(input.to_owned());

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap_or_else(|err| {
            eprintln!("Error: {:?}", err);
            std::process::exit(1);
        });

        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        let expected_identifiers = ["x", "y", "foobar"];

        for (i, test) in expected_identifiers.iter().enumerate() {
            let statement = program.statements[i].as_ref();
            assert_eq!(statement.token_literal(), "let");

            let let_statement = statement
                .as_any()
                .downcast_ref::<ast::LetStatement>()
                .unwrap();
            assert_eq!(let_statement.name.as_ref().unwrap().value, *test);
        }
    }

    /// Checks if the parser has encountered any errors and panics if it has.
    ///
    /// # Arguments
    ///
    /// * `parser` - The parser to check for errors.
    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        if errors.is_empty() {
            return;
        }

        eprintln!("parser has {} errors", errors.len());
        for error in errors {
            eprintln!("parser error: {}", error);
        }

        panic!("parser has errors");
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;"#;

        let lexer = Lexer::new(input.to_owned());

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap_or_else(|err| {
            eprintln!("Error: {:?}", err);
            std::process::exit(1);
        });

        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        for statement in program.statements {
            assert_eq!(statement.token_literal(), "return");
        }
    }
}
