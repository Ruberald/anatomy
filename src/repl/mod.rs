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

    fn parse_hex(&mut self, i: &str) -> Result<Vec<u8>, std::num::ParseIntError>{
        let split = i.split(" ").collect::<Vec<&str>>();
        let mut results: Vec<u8> = vec![];
        for hex_string in split {
            let byte = u8::from_str_radix(&hex_string, 16);
            match byte {
                Ok(result) => {
                    results.push(result);
                },
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(results)
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

                ".program" => {
                    println!("Listing instructions currently in VM's program vector:");
                    for instruction in &self.vm.program {
                        println!("{}", instruction);
                    }
                    println!("End of Program Listing");
                },

                ".registers" => {
                    println!("Listing registers and all contents:");
                    println!("{:#?}", self.vm.registers);
                    println!("End of Register Listing")
                },

                _ => {
                    let results = self.parse_hex(trim_buffer);
                    match results {
                        Ok(mut bytes) => {
                            self.vm.program.append(&mut bytes);
                        },
                        Err(_e) => {
                            println!("Unable to decode hex string. Please enter 4 groups of 2 hex characters.")
                        }
                    };
                    self.vm.run_once();
                }
            }

            buffer.clear();
        }
    }
}
