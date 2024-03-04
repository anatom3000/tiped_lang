use std::{error::Error, fs, io::Write};

mod lexer;
mod parser;
mod tree;
mod typer;

fn process_file(path: String) -> Result<typer::Environment, Box<dyn Error>> {
    let source = fs::read_to_string(path)?;
    let mut env = typer::Environment::new();

    process(&source, &mut env);

    Ok(env)
}

fn process(source: &str, env: &mut typer::Environment) {
    let tokens = lexer::Lexer::new(&source).lex();

    assert!(tokens.errors.is_empty());

    let decls = parser::Parser::new(tokens.tokens).parse();

    for d in decls {
        env.declaration(d);
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut env = match std::env::args().nth(1) {
        Some(source_file_path) => process_file(source_file_path)?,
        None => typer::Environment::new(),
    };

    let stdin = std::io::stdin();
    let mut input = String::new();
    loop {
        input.clear();
        print!("#> ");
        std::io::stdout().flush()?;

        while &input[(input.len() as isize - 2).max(0) as usize..] != ";\n" {
            stdin.read_line(&mut input)?;
        }

        input.pop();
        input.pop();

        process(&input, &mut env);
    }
}
