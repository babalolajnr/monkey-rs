use lexer::lib::Lexer;

pub mod lexer;
pub mod token;

fn main() {
    let mut lexer = Lexer::new("let == 5;".to_string());

    println!("{:?}", lexer.next_token());
    println!("{:?}", lexer.next_token());
    println!("{:?}", lexer.next_token());
    println!("{:?}", lexer.next_token());
    println!("{:?}", lexer.next_token());
}
