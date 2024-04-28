use crate::token::lib::{lookup_ident, Token, TokenType};

/// The `Lexer` struct represents a lexer that tokenizes input strings.
pub struct Lexer {
    input: String,
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
            input,
            position: 0,
            read_position: 0,
            character: 0 as char,
        };

        lexer.read_char();
        lexer
    }

    /// Reads the next character from the input string and updates the lexer's state.
    fn read_char(&mut self) {
        self.character = self
            .input
            .chars()
            .nth(self.read_position)
            .unwrap_or(0 as char);
        self.position = self.read_position;
        self.read_position += 1;
    }

    /// Retrieves the next token from the input string.
    ///
    /// # Returns
    ///
    /// The next token as a `Token` enum variant.
    pub fn next_token(&mut self) -> Token {
        self.consume_whitespace();

        match self.character {
            '=' | '!' => {
                let token = self.handle_double_char_token();
                self.read_char();
                token
            }
            '+' | '-' | '/' | '*' | '<' | '>' | ';' | ',' | '{' | '}' | '(' | ')' => {
                let token = self.handle_single_char_token();
                self.read_char();
                token
            }
            '\u{0}' => Token::new(TokenType::EOF, "".to_string()),
            _ => self.handle_identifier_or_number(),
        }
    }

    /// Handles double character tokens like `==` and `!=`.
    ///
    /// # Returns
    ///
    /// The token as a `Token` enum variant.
    fn handle_double_char_token(&mut self) -> Token {
        let token_type = self.get_double_char_token_type();
        let literal = match token_type {
            TokenType::EQ => "==".to_string(),
            TokenType::NOTEQ => "!=".to_string(),
            TokenType::ASSIGN => "=".to_string(),
            TokenType::BANG => "!".to_string(),
            _ => panic!("Unexpected token type"),
        };
        if matches!(token_type, TokenType::EQ | TokenType::NOTEQ) {
            self.read_char();
        }
        Token::new(token_type, literal)
    }

    /// Determines the token type for double character tokens.
    ///
    /// # Returns
    ///
    /// The token type as a `TokenType` enum variant.
    fn get_double_char_token_type(&mut self) -> TokenType {
        match (self.character, self.peek()) {
            ('=', '=') => TokenType::EQ,
            ('!', '=') => TokenType::NOTEQ,
            ('=', _) => TokenType::ASSIGN,
            ('!', _) => TokenType::BANG,
            (ch, _) => panic!("Unexpected character: {}", ch),
        }
    }

    /// Handles single character tokens.
    ///
    /// # Returns
    ///
    /// The token as a `Token` enum variant.
    fn handle_single_char_token(&mut self) -> Token {
        let token_type = match self.character {
            '+' => TokenType::PLUS,
            '-' => TokenType::MINUS,
            '/' => TokenType::SLASH,
            '*' => TokenType::ASTERISK,
            '<' => TokenType::LT,
            '>' => TokenType::GT,
            ';' => TokenType::SEMICOLON,
            ',' => TokenType::COMMA,
            '{' => TokenType::LBRACE,
            '}' => TokenType::RBRACE,
            '(' => TokenType::LPAREN,
            ')' => TokenType::RPAREN,
            _ => panic!("Unexpected character: {}", self.character),
        };

        Token::new(token_type, self.character.to_string())
    }

    /// Handles identifiers or numbers.
    ///
    /// # Returns
    ///
    /// The token as a `Token` enum variant.
    fn handle_identifier_or_number(&mut self) -> Token {
        if is_letter(self.character) {
            let literal = self.read_identifier();
            let token_type = lookup_ident(&literal);
            Token::new(token_type, literal)
        } else if self.character.is_ascii_digit() {
            Token::new(TokenType::INT, self.read_number())
        } else {
            Token::new(TokenType::ILLEGAL, self.character.to_string())
        }
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

        self.input[position..self.position].to_string()
    }

    /// Reads a number from the input string.
    ///
    /// # Returns
    ///
    /// The number as a `String`.
    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.character.is_ascii_digit() {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    /// Consumes whitespace characters from the input string.
    fn consume_whitespace(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.read_char();
        }
    }

    /// Peeks the next character in the input string without consuming it.
    ///
    /// # Returns
    ///
    /// The next character as a `char`. If there are no more characters, `'\0'` is returned.
    fn peek(&self) -> char {
        self.input
            .chars()
            .nth(self.read_position)
            .unwrap_or(0 as char)
    }
}

fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
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
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
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
            Token::new(TokenType::NOTEQ, "!=".to_string()),
            Token::new(TokenType::INT, "9".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for expected_token in expected_tokens {
            let token = lexer.next_token();
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
    fn test_read_char() {
        let input = "let five = 5;";
        let mut lexer = Lexer::new(input.to_string());

        let expected_characters = vec![
            'l', 'e', 't', ' ', 'f', 'i', 'v', 'e', ' ', '=', ' ', '5', ';',
        ];

        for expected_character in expected_characters {
            assert_eq!(expected_character, lexer.character);
            lexer.read_char();
        }
    }

    #[test]
    fn test_handle_double_char_token() {
        let input = "let five == 5;";
        let mut lexer = Lexer::new(input.to_string());

        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
        ];

        for expected_token in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(expected_token, token);
        }
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
            let token = lexer.next_token();
            assert_eq!(expected_token, token);
        }
    }

    #[test]
    fn test_next_token_with_whitespace_and_newlines() {
        let input = "let five = 5;\nlet ten = 10 ;";
        let mut lexer = Lexer::new(input.to_string());

        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
        ];

        for expected_token in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(expected_token, token);
        }
    }

    #[test]
    fn test_next_token_with_delimiter() {
        let input = "let;";
        let mut lexer = Lexer::new(input.to_string());

        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        for expected_token in expected_tokens {
            let token = lexer.next_token();
            assert_eq!(expected_token, token);
        }
    }
}
