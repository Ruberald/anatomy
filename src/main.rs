mod lexer;

mod vm;
mod instruction;

mod repl;

fn main() {
    let mut repl = repl::REPL::new();

    repl.run();
}
