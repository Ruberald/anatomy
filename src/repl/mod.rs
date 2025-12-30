use std::io;
use std::io::Write;

use crate::ast::Node;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::lexer::TokenType;
use crate::parser::check_parser_errors;
use crate::vm::VM;

pub enum ReplMode {
    Hex,
    Normal,
}

pub struct REPL {
    command_buffer: Vec<String>,
    vm: VM,
    compiler: Compiler,
    mode: ReplMode,
}

impl REPL {
    pub fn new() -> REPL {
        REPL {
            vm: VM::new(vec![], vec![]),
            command_buffer: vec![],
            compiler: Compiler::new(),
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
                    println!("{:#?}", self.vm.frames[0].registers);
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

                        println!("Parsed tree:");
                        let mut parser = crate::parser::Parser::new(lexer);
                        let program = parser.parse_program();
                        println!("{}", program.string());
                        check_parser_errors(&parser);

                        // Compile program into VM bytecode and run it
                        let (bytes, pool) = crate::compiler::compile_program(&program);
                        if !bytes.is_empty() {
                            // Replace VM program with compiled bytes and run
                            self.vm.program = bytes;
                            self.vm.constant_pool = pool;
                            self.vm.run();
                            println!("Registers after run: {:#?}", self.vm.frames[0].registers);
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
        // LOAD R0 constant_pool_index(0) -> 00 00 00 00
        repl.vm.constant_pool.push(10);
        let mut bytes = repl.parse_hex("00 00 00 00").unwrap();
        repl.vm.program.append(&mut bytes);

        // LOAD R1 constant_pool_index(1) -> 00 01 00 01
        repl.vm.constant_pool.push(20);
        let mut bytes = repl.parse_hex("00 01 00 01").unwrap();
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
        assert_eq!(repl.vm.frames[0].registers[0], 0x0A, "R0 should be 10");
        assert_eq!(repl.vm.frames[0].registers[1], 0x14, "R1 should be 20");
        assert_eq!(repl.vm.frames[0].registers[2], 0x1E, "R2 should contain 30");
    }

    // -------------------------------
    // VM INTEGRATION TEST
    // Ensure normal mode program is run properly
    // -------------------------------
    #[test]
    fn test_repl_normal_program_execution_load_and_add() {
        use crate::repl::REPL;

        let mut repl = REPL::new();

        let input_program = "
            let f = false;
            let t = true;
            let a = 10;
            let b = 20;
            let c = a + b;
        ";
        // let d = t && f;

        let mut lexer = Lexer::new(input_program.to_string());
        let mut parser = crate::parser::Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        let (bytes, pool) = crate::compiler::compile_program(&program);
        repl.vm.program = bytes;
        repl.vm.constant_pool = pool;

        // Run the program
        repl.vm.run();

        // Validate results
        assert_eq!(repl.vm.frames[0].registers[0], 0, "R1 should be false (0)");
        assert_eq!(repl.vm.frames[0].registers[1], 1, "R0 should be true (1)");
        assert_eq!(repl.vm.frames[0].registers[2], 10, "R2 should be 10");
        assert_eq!(repl.vm.frames[0].registers[3], 20, "R3 should be 20");
        assert_eq!(
            repl.vm.frames[0].registers[4], 30,
            "R4 should be 30 from 10 + 20"
        );
        // assert_eq!( repl.vm.frames[0].registers[5], 0, "R5 should be false (0) from true && false");
    }
}
