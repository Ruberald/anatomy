use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::ast::Node;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
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

        // Use rustyline for GNU Readline-like features (line editing, history)
        let mut rl = DefaultEditor::new().expect("Failed to create rustyline Editor");
        let hist_path = ".anatomy_history";
        let _ = rl.load_history(hist_path);

        loop {
            let readline = rl.readline("~> ");
            match readline {
                Ok(line) => {
                    let trim_buffer = line.trim();
                    // record history and command buffer
                    if !trim_buffer.is_empty() {
                        let _ = rl.add_history_entry(trim_buffer);
                    }
                    self.command_buffer.push(trim_buffer.to_string());

                    match trim_buffer {
                        ".quit" => {
                            println!("Exiting.");
                            let _ = rl.save_history(hist_path);
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
                                        println!(
                                            "Invalid hex input. Expected space-separated bytes like: 01 ef a3 10"
                                        );
                                    }
                                }
                                self.vm.run_once();
                            }

                            ReplMode::Normal => {
                                let mut lexer = Lexer::new(trim_buffer.to_string());

                                log::debug!("Parsed tree:");
                                let mut parser = crate::parser::Parser::new(lexer);
                                let program = parser.parse_program();
                                log::debug!("{}", program.string());
                                check_parser_errors(&parser);

                                // Compile program into VM bytecode and run it
                                let (bytes, pool, last_expr_reg) =
                                    self.compiler.compile_statements(&program);
                                // Replace VM program with compiled bytes and run
                                self.vm.program = bytes;
                                self.vm.constant_pool = pool;
                                self.vm.frames.last_mut().unwrap().pc = 0;
                                self.vm.run();

                                log::debug!(
                                    "Registers after run: {:#?}",
                                    self.vm.frames[0].registers
                                );

                                if let Some(r) = last_expr_reg {
                                    println!("=> {}", self.vm.frames[0].registers[r as usize]);
                                } else {
                                    // italics: no value to print (e.g., statement)
                                    println!("\x1b[3m=> no value\x1b[0m");
                                }
                            }
                        },
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    // User pressed Ctrl-C — ignore and continue
                    continue;
                }
                Err(ReadlineError::Eof) => {
                    // Ctrl-D / EOF — save history and exit
                    let _ = rl.save_history(hist_path);
                    println!("Exiting.");
                    std::process::exit(0);
                }
                Err(err) => {
                    println!("Error reading line: {err}");
                    break;
                }
            }
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
            let d = a * b + c;
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
        assert_eq!(
            repl.vm.frames[0].registers[5], 230,
            "R5 should be 230 from 10 * 20 + 30"
        );
        // assert_eq!( repl.vm.frames[0].registers[5], 0, "R5 should be false (0) from true && false");
    }

    #[test]
    fn test_normal_mode_complex_expression() {
        let mut repl = REPL::new();

        let input_program = "
            let result = (5 + 10) * (2 + 3);
        ";

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
        assert_eq!(
            repl.vm.frames[0].registers[0], 75,
            "R0 should be 75 from (5 + 10) * (2 + 3)"
        );
    }

    #[test]
    fn test_repl_if_expression_in_let_runtime() {
        let mut repl = REPL::new();

        let input_program = "
            let x = if (true) { 10 } else { 20 };
            let y = if (false) { 10 } else { 20 };
            let z = if (10 > 5) { 30 } else { 40 };
            let zz = if (5 > 10) { 30 } else { 40 };
        ";

        let mut lexer = Lexer::new(input_program.to_string());
        let mut parser = crate::parser::Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        let (bytes, pool, _) = repl.compiler.compile_statements(&program);
        repl.vm.program = bytes;
        repl.vm.constant_pool = pool;

        // Run the compiled program on the VM
        repl.vm.run();

        // 'x' was registered by the compiler; look up its register index
        let reg_x = repl
            .compiler
            .get_symbol("x")
            .expect("symbol x should exist");

        let reg_y = repl
            .compiler
            .get_symbol("y")
            .expect("symbol y should exist");

        let reg_z = repl
            .compiler
            .get_symbol("z")
            .expect("symbol z should exist");

        let reg_zz = repl
            .compiler
            .get_symbol("zz")
            .expect("symbol zz should exist");

        assert_eq!(
            repl.vm.frames[0].registers[reg_x as usize], 10,
            "x should be 10"
        );
        assert_eq!(
            repl.vm.frames[0].registers[reg_y as usize], 20,
            "y should be 20"
        );
        assert_eq!(
            repl.vm.frames[0].registers[reg_z as usize], 30,
            "z should be 30"
        );
        assert_eq!(
            repl.vm.frames[0].registers[reg_zz as usize], 40,
            "zz should be 40"
        );
    }

    #[test]
    fn test_repl_if_statement_with_let_inside_runtime() {
        let mut repl = REPL::new();

        let input_program = "
            if (true) { let x = 10; };
            if (false) { let x = 20; };
            if (false) { let y = 30; };;
        ";

        let mut lexer = Lexer::new(input_program.to_string());
        let mut parser = crate::parser::Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        let (bytes, pool, _) = repl.compiler.compile_statements(&program);
        repl.vm.program = bytes;
        repl.vm.constant_pool = pool;

        repl.vm.run();

        let reg = repl
            .compiler
            .get_symbol("x")
            .expect("symbol x should exist");

        assert_eq!(
            repl.vm.frames[0].registers[reg as usize], 10,
            "inner x should be 10"
        );

        let reg_y = repl.compiler.get_symbol("y");
        assert!(reg_y.is_none(), "symbol y should not exist");
    }
}
