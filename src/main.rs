mod ast;
mod lexer;
mod parser;

mod instruction;
mod vm;

mod repl;

fn main() {
    let mut repl = repl::REPL::new();

    repl.run();
}
