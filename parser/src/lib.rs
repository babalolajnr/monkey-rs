/// The `parser` module contains the implementation of the Monkey programming language parser.
pub mod ast;

use std::collections::HashMap;

use anyhow::{anyhow, Ok, Result};
use lexer::{
    token::{self, Token, TokenType},
    Lexer,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest = 1,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

/// The `Parser` struct represents the Monkey parser.
#[derive(Debug, Clone)]
pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,
}

type PrefixParseFn = fn(parser: &mut Parser) -> Box<dyn ast::Expression>;
type InfixParseFn = fn(Box<dyn ast::Expression>) -> Box<dyn ast::Expression>;

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
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.next_token();
        parser.next_token();

        parser.register_prefix(TokenType::IDENT, Parser::parse_identifier);
        parser.register_prefix(TokenType::INT, Parser::parse_integer_literal);
        parser.register_prefix(TokenType::MINUS, Parser::parse_prefix_expression);
        parser.register_prefix(TokenType::BANG, Parser::parse_prefix_expression);

        parser
    }

    /// Registers a prefix parse function for the given token type.
    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    /// Registers an infix parse function for the given token type.
    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    /// Parses an identifier and returns it as a boxed trait object.
    fn parse_identifier(&mut self) -> Box<dyn ast::Expression> {
        Box::new(ast::Identifier {
            token: self.current_token.clone().expect("No current token"),
            value: self
                .current_token
                .as_ref()
                .expect("No current literal found")
                .literal
                .clone(),
        })
    }

    /// Parses a prefix expression and returns it as a boxed trait object.
    fn parse_prefix_expression(&mut self) -> Box<dyn ast::Expression> {
        let token = self
            .current_token
            .as_ref()
            .ok_or_else(|| anyhow!("No current token"))
            .unwrap();

        let operator = token.literal.clone();
        let token_clone = token.clone();

        self.next_token();

        Box::new(ast::PrefixExpression {
            token: token_clone,
            operator,
            right: self.parse_expression(Precedence::Prefix).unwrap(),
        })
    }

    /// Reports an error for an unexpected token.
    fn no_prefix_parse_fn_error(&mut self, token_type: TokenType) {
        self.errors
            .push(format!("no prefix parse function for {:?}", token_type));
    }

    /// Parse an expression with the given precedence.
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn ast::Expression>> {
        let token_type = &self.current_token.as_ref().unwrap().token_type;

        let prefix = self.prefix_parse_fns.get(token_type);

        match prefix {
            Some(prefix) => {
                let left_exp = prefix(self);
                Some(left_exp)
            }
            None => {
                self.no_prefix_parse_fn_error(*token_type);
                None
            }
        }
    }

    fn parse_integer_literal(&mut self) -> Box<dyn ast::Expression> {
        let token = self.current_token.as_ref().unwrap();
        let value = token
            .literal
            .parse::<i64>()
            .unwrap_or_else(|_| panic!("Could not parse {} as integer", token.literal));

        Box::new(ast::IntegerLiteral {
            token: token.clone(),
            value,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<ast::ExpressionStatement> {
        let statement = ast::ExpressionStatement {
            token: self.current_token.clone(),
            expression: Some(self.parse_expression(Precedence::Lowest).unwrap()),
        };

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(statement)
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
            _ => self
                .parse_expression_statement()
                .map(box_statement)
                .map_err(|err| anyhow!("Failed to parse expression statement: {:?}", err)),
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
        let token = self.current_token.as_ref()?;
        let mut statement = ast::LetStatement {
            token: Some(token.clone()),
            name: None,
            value: None,
        };

        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        let token = self.current_token.as_ref()?;
        statement.name = Some(ast::Identifier {
            token: token.clone(),
            value: token.literal.clone(),
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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input.to_owned());

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap_or_else(|err| {
            eprintln!("Error: {:?}", err);
            std::process::exit(1);
        });

        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].as_ref();
        assert_eq!(statement.token_literal(), "foobar");

        let expression_statement = statement
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap();
        assert_eq!(
            expression_statement.expression.as_ref().unwrap().string(),
            "foobar"
        );
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.to_owned());

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program().unwrap_or_else(|err| {
            eprintln!("Error: {:?}", err);
            std::process::exit(1);
        });

        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].as_ref();
        assert_eq!(statement.token_literal(), "5");

        let expression_statement = statement
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap();
        assert_eq!(
            expression_statement.expression.as_ref().unwrap().string(),
            "5"
        );
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = [("!5;", "!", "5"), ("-15;", "-", "15")];

        for (input, operator, value) in prefix_tests.iter() {
            let lexer = Lexer::new(input.to_string());

            let mut parser = Parser::new(lexer);

            let program = parser.parse_program().unwrap_or_else(|err| {
                eprintln!("Error: {:?}", err);
                std::process::exit(1);
            });

            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            let stmt = program.statements[0]
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .unwrap_or_else(|| {
                    panic!(
                        "program.statements[0] is not ast::ExpressionStatement. got={:?}",
                        program.statements[0]
                    )
                });

            let exp = stmt
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<ast::PrefixExpression>()
                .unwrap_or_else(|| {
                    panic!(
                        "stmt is not ast::PrefixExpression. got={:?}",
                        stmt.expression
                    )
                });

            assert_eq!(
                exp.operator, *operator,
                "exp.Operator is not '{}'. got={}",
                operator, exp.operator
            );

            if !test_integer_literal(
                exp.right.as_ref(),
                value.parse::<i64>().expect("Could not parse value as i64"),
            ) {
                return;
            }
        }
    }

    fn test_integer_literal(il: &dyn ast::Expression, value: i64) -> bool {
        let int_lit = il
            .as_any()
            .downcast_ref::<ast::IntegerLiteral>()
            .unwrap_or_else(|| panic!("il not ast::IntegerLiteral. got={:?}", il));

        assert_eq!(
            int_lit.value, value,
            "int_lit.value not {}. got={}",
            value, int_lit.value
        );

        assert_eq!(
            ast::Node::token_literal(int_lit),
            value.to_string(),
            "int_lit.token_literal not {}. got={}",
            value,
            ast::Node::token_literal(int_lit)
        );

        true
    }
}
