use anyhow::{Ok, Result};

use crate::token::lib::{lookup_ident, Token, TokenType};

/// The `Lexer` struct represents a lexer that tokenizes input strings.
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    character: char,
}

impl Lexer {
    /// Creates a new `Lexer` instance with the given input string.
    ///
    /// # Arguments
    ///
    /// * `input` - The input string to tokenize.
    ///
    /// # Returns
    ///
    /// A new `Lexer` instance.
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            character: '\0',
        };

        lexer.read_char();
        lexer
    }

    /// Reads the next character from the input string and updates the lexer's state.
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.character = '\0';
        } else {
            self.character = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    /// Returns the next token from the input string.
    ///
    /// # Returns
    ///
    /// The next token as a `Token` enum variant.
    fn next_token(&mut self) -> Result<Token> {
        self.consume_whitespace();

        let token = match self.character {
            '=' => {
                if self.peek() == '=' {
                    self.read_char();
                    Ok(Token::new(TokenType::EQ, "==".to_string()))
                } else {
                    Ok(Token::new(TokenType::ASSIGN, self.character.to_string()))
                }
            }
            '+' => Ok(Token::new(TokenType::PLUS, self.character.to_string())),
            '-' => Ok(Token::new(TokenType::MINUS, self.character.to_string())),
            '!' => {
                if self.peek() == '=' {
                    self.read_char();
                    Ok(Token::new(TokenType::NOTEQ, "!=".to_string()))
                } else {
                    Ok(Token::new(TokenType::BANG, self.character.to_string()))
                }
            }
            '/' => Ok(Token::new(TokenType::SLASH, self.character.to_string())),
            '*' => Ok(Token::new(TokenType::ASTERISK, self.character.to_string())),
            '<' => Ok(Token::new(TokenType::LT, self.character.to_string())),
            '>' => Ok(Token::new(TokenType::GT, self.character.to_string())),
            ';' => Ok(Token::new(TokenType::SEMICOLON, self.character.to_string())),
            ',' => Ok(Token::new(TokenType::COMMA, self.character.to_string())),
            '{' => Ok(Token::new(TokenType::LBRACE, self.character.to_string())),
            '}' => Ok(Token::new(TokenType::RBRACE, self.character.to_string())),
            '(' => Ok(Token::new(TokenType::LPAREN, self.character.to_string())),
            ')' => Ok(Token::new(TokenType::RPAREN, self.character.to_string())),
            '\0' => Ok(Token::new(TokenType::EOF, "".to_string())),
            _ => {
                if is_letter(self.character) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);

                    Ok(Token::new(token_type, literal))
                } else if is_digit(self.character) {
                    Ok(Token::new(TokenType::INT, self.read_number()))
                } else {
                    Ok(Token::new(TokenType::ILLEGAL, self.character.to_string()))
                }
            }
        };

        // if self.peek().is_whitespace() {
        self.read_char();
        // }

        token
    }

    /// Reads an identifier from the input string.
    ///
    /// # Returns
    ///
    /// The identifier as a `String`.
    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.character) {
            self.read_char();
        }

        self.input[position..self.position].iter().collect()
    }

    /// Reads a number from the input string.
    ///
    /// # Returns
    ///
    /// The number as a `String`.
    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.character) {
            self.read_char();
        }

        self.input[position..self.position].iter().collect()
    }

    /// Consumes whitespace characters from the input string.
    fn consume_whitespace(&mut self) {
        while self.character.is_whitespace() {
            self.read_char();
        }
    }

    /// Peeks the next character in the input string without consuming it.
    ///
    /// # Returns
    ///
    /// The next character as a `Result<char>`. If there are no more characters, `Ok('\0')` is returned.
    fn peek(&mut self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        }

        self.input[self.read_position]
    }
}

fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "let five= 5;
        let ten = 10;
        
        let add = fn(x, y) {
          x + y ;
        };
        
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        
        10 == 10;
        10 != 9;";

        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::FUNCTION, "fn".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "result".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::BANG, "!".to_string()),
            Token::new(TokenType::MINUS, "-".to_string()),
            Token::new(TokenType::SLASH, "/".to_string()),
            Token::new(TokenType::ASTERISK, "*".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::GT, ">".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::NOTEQ, "!=".to_string()),
            Token::new(TokenType::INT, "9".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for expected_token in expected_tokens {
            let token = lexer.next_token().unwrap();
            assert_eq!(expected_token, token);
        }
    }

    #[test]
    fn test_read_identifier() {
        let input = "foobar";
        let mut lexer = Lexer::new(input.to_string());

        let expected = "foobar";
        let actual = lexer.read_identifier();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_read_identifier_with_underscores() {
        let input = "foo_bar";
        let mut lexer = Lexer::new(input.to_string());

        let expected = "foo_bar";
        let actual = lexer.read_identifier();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_read_identifier_joined_with_special_characters() {
        let input = "foo!bar";
        let mut lexer = Lexer::new(input.to_string());

        let expected = "foo";
        let actual = lexer.read_identifier();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_next_token_with_whitespace() {
        let input = "let five = 5;";
        let mut lexer = Lexer::new(input.to_string());

        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
        ];

        for expected_token in expected_tokens {
            let token = lexer.next_token().unwrap();
            assert_eq!(expected_token, token);
        }
    }
}
