fn main() {
    let user = std::env::var("USER").unwrap_or_else(|_| "Unknown User".to_string());

    println!("Hello, {}! This is the Monkey programming language!", user);
    println!("Feel free to type in commands");
    repl::start();
}
