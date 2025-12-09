use std::io;
use std::io::Write;

use crate::lexer::Lexer;
use crate::lexer::TokenType;
use crate::vm::VM;

pub enum ReplMode {
    Hex,
    Normal,
}

pub struct REPL {
    command_buffer: Vec<String>,
    vm: VM,
    mode: ReplMode,
}

impl REPL {
    pub fn new() -> REPL {
        REPL {
            vm: VM::new(),
            command_buffer: vec![],
            mode: ReplMode::Normal,
        }
    }

    fn parse_hex(&mut self, i: &str) -> Result<Vec<u8>, std::num::ParseIntError> {
        let split = i.split_whitespace().collect::<Vec<&str>>();
        let mut results: Vec<u8> = vec![];
        for hex_string in split {
            let byte = u8::from_str_radix(hex_string, 16);
            match byte {
                Ok(result) => {
                    results.push(result);
                }
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

            stdin
                .read_line(&mut buffer)
                .expect("Unable to read line from repl.");

            self.command_buffer.push(buffer.to_string());

            let trim_buffer = buffer.trim();
            match trim_buffer {
                ".quit" => {
                    println!("Exiting.");
                    std::process::exit(0);
                }

                ".hex" => {
                    self.mode = ReplMode::Hex;
                    println!("Switched to HEX mode. Raw byte input enabled.");
                }

                ".normal" => {
                    self.mode = ReplMode::Normal;
                    println!("Switched to NORMAL mode.");
                }

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
                }

                ".registers" => {
                    println!("Listing registers and all contents:");
                    println!("{:#?}", self.vm.registers);
                    println!("End of Register Listing")
                }

                _ => match self.mode {
                    ReplMode::Hex => {
                        let results = self.parse_hex(trim_buffer);
                        match results {
                            Ok(mut bytes) => {
                                self.vm.program.append(&mut bytes);
                            }
                            Err(_) => {
                                println!("Invalid hex input. Expected space-separated bytes like: 01 ef a3 10");
                            }
                        }
                        self.vm.run_once();
                    }

                    ReplMode::Normal => {
                        let mut lexer = Lexer::new(trim_buffer.to_string());

                        println!("Tokens:");
                        while let token = lexer.next_token() {
                            if token.token_type == TokenType::EOF {
                                break;
                            }
                            println!("{:?}", token);
                        }
                    }
                },
            }

            buffer.clear();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // -------------------------------
    // parse_hex() TESTS
    // -------------------------------
    #[test]
    fn test_parse_hex_valid() {
        let mut repl = REPL::new();
        let input = "01 0A ff";
        let parsed = repl.parse_hex(input).unwrap();
        assert_eq!(parsed, vec![0x01, 0x0A, 0xFF]);
    }

    #[test]
    fn test_parse_hex_single_byte() {
        let mut repl = REPL::new();
        let parsed = repl.parse_hex("7F").unwrap();
        assert_eq!(parsed, vec![0x7F]);
    }

    #[test]
    fn test_parse_hex_with_extra_spaces() {
        let mut repl = REPL::new();
        let parsed = repl.parse_hex("  10   20  30 ").unwrap();
        assert_eq!(parsed, vec![0x10, 0x20, 0x30]);
    }

    #[test]
    fn test_parse_hex_invalid() {
        let mut repl = REPL::new();
        let result = repl.parse_hex("xz 10 20");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_hex_empty_string() {
        let mut repl = REPL::new();
        let parsed = repl.parse_hex("").unwrap();
        assert_eq!(parsed.len(), 0);
    }

    // -------------------------------
    // VM-INTEGRATION TEST
    // Ensure parsed hex program is run properly
    // -------------------------------
    #[test]
    fn test_repl_hex_program_execution_load_and_add() {
        use crate::repl::REPL;

        let mut repl = REPL::new();

        // LOAD R0 = 10   → 00 00 00 0A
        let mut bytes = repl.parse_hex("00 00 00 0A").unwrap();
        repl.vm.program.append(&mut bytes);

        // LOAD R1 = 20   → 00 01 00 14
        let mut bytes = repl.parse_hex("00 01 00 14").unwrap();
        repl.vm.program.append(&mut bytes);

        // ADD R2 = R0 + R1 → 01 00 01 02
        let mut bytes = repl.parse_hex("01 00 01 02").unwrap();
        repl.vm.program.append(&mut bytes);

        // HLT → 05
        let mut bytes = repl.parse_hex("05").unwrap();
        repl.vm.program.append(&mut bytes);

        // Run the program
        repl.vm.run();

        // Validate results
        assert_eq!(repl.vm.registers[0], 0x0A, "R0 should be 10");
        assert_eq!(repl.vm.registers[1], 0x14, "R1 should be 20");
        assert_eq!(repl.vm.registers[2], 0x1E, "R2 should contain 30");
    }
}
