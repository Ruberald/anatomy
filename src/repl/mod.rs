use std;
use std::io;
use std::io::Write;

use crate::vm::VM;

pub struct REPL {
    command_buffer: Vec<String>,
    vm : VM,
}

impl REPL {
    pub fn new() -> REPL {
        REPL {
            vm: VM::new(),
            command_buffer: vec![],
        }
    }

    pub fn run(&mut self) {
        const VERSION: &str = env!("CARGO_PKG_VERSION");
        println!("Anatomy REPL {VERSION}");

        let mut buffer = String::new();


        loop {
            let stdin = io::stdin();

            print!("~> ");
            io::stdout().flush().expect("Unable to flush stdout.");

            stdin.read_line(&mut buffer).expect("Unable to read line from repl.");

            self.command_buffer.push(buffer.to_string());

            let trim_buffer = buffer.trim();
            match trim_buffer {
                ".quit" => {
                    println!("Exiting.");
                    std::process::exit(0);
                },

                ".history" => {
                    for command in &self.command_buffer {
                        println!("{}", command);
                    }
                }

                _ => {
                    println!("Invalid expression, command or input.");
                }
            }

            buffer.clear();
        }
    }
}
