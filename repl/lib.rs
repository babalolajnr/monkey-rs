use lexer::{token::TokenType, Lexer};

pub fn start() {
    const PROMPT: &str = ">> ";

    loop {
        print!("{}", PROMPT);

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if input.trim() == "exit" {
            break;
        }

        let mut lexer = Lexer::new(input);

        let mut tok = lexer.next_token();

        while tok.token_type != TokenType::EOF {
            println!("{:?}", tok);
            tok = lexer.next_token();
        }
    }
}
