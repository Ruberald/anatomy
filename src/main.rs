mod ast;
mod lexer;
mod parser;

mod compiler;
mod instruction;
mod vm;

mod repl;

fn main() {
    // Initialize logger for debug output. Configure the level via RUST_LOG.
    env_logger::init();

    let mut repl = repl::REPL::new();
    repl.run();
}
