use crate::ast::*;
use crate::instruction::Opcode;
use std::collections::HashMap;

pub struct Compiler {
    pub program: Vec<u8>,
    next_register: u8,
    symbols: HashMap<String, u8>,
    constant_pool: Vec<i64>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            program: vec![],
            next_register: 0,
            symbols: HashMap::new(),
            constant_pool: vec![],
        }
    }
    pub fn compile_statements(&mut self, program: &Program) -> (Vec<u8>, Vec<i64>) {
        self.program.clear();

        for stmt in &program.statements {
            self.compile_statement(stmt);
        }

        // Ensure program halts when done
        self.program.push(Opcode::HLT as u8);

        println!("next_register: {}", self.next_register);
        println!("symbols: {:?}", self.symbols);
        println!("bytes: {:?}", self.program);
        println!("constant_pool: {:?}", self.constant_pool);

        (self.program.clone(), self.constant_pool.clone())
    }

    /// Entry point: compile a Program into raw bytes for the VM.
    pub fn compile_program(mut self, program: &Program) -> (Vec<u8>, Vec<i64>) {
        for stmt in &program.statements {
            self.compile_statement(stmt);
        }

        // Ensure program halts when done
        self.program.push(Opcode::HLT as u8);

        (self.program, self.constant_pool)
    }

    fn alloc_register(&mut self) -> u8 {
        let r = self.next_register;
        self.next_register = self.next_register.wrapping_add(1);
        r
    }

    /// Return the register index for a named symbol, if present.
    pub fn get_symbol(&self, name: &str) -> Option<u8> {
        self.symbols.get(name).copied()
    }

    fn compile_statement(&mut self, stmt: &Box<dyn Statement>) {
        // Downcast to concrete statements we support
        if let Some(let_stmt) = stmt.as_any().downcast_ref::<LetStatement>() {
            // Allocate a register for the `let` binding and compile the value into it.
            let reg = self.alloc_register();
            let _ = self.compile_expression_dest(&let_stmt.value, Some(reg));
            self.symbols.insert(let_stmt.name.value.clone(), reg);
            return;
        }

        if let Some(expr_stmt) = stmt.as_any().downcast_ref::<ExpressionStatement>() {
            // compile expression and leave result in a register (no-op beyond that)
            let _ = self.compile_expression(&expr_stmt.expression);
            return;
        }

        if let Some(_ret) = stmt.as_any().downcast_ref::<ReturnStatement>() {
            // No call/return support in the current VM; ignore or HLT
            self.program.push(Opcode::HLT as u8);
            return;
        }

        if let Some(_block) = stmt.as_any().downcast_ref::<BlockStatement>() {
            // Not implemented yet; no-op
        }
    }

    /// Compile an expression and return the register index holding the result.
    fn compile_expression(&mut self, expr: &Box<dyn Expression>) -> u8 {
        self.compile_expression_dest(expr, None)
    }

    /// Compile an expression, optionally writing the result into `into` register.
    /// Returns the register index containing the result.
    fn compile_expression_dest(&mut self, expr: &Box<dyn Expression>, into: Option<u8>) -> u8 {
        if let Some(int_lit) = expr.as_any().downcast_ref::<IntegerLiteral>() {
            let reg = if let Some(d) = into {
                d
            } else {
                self.alloc_register()
            };
            // LOAD reg constant_pool_index
            self.constant_pool.push(int_lit.value);
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
            let index = (self.constant_pool.len() - 1) as u16;
            self.program.push(((index >> 8) & 0xFF) as u8);
            self.program.push((index & 0xFF) as u8);
            return reg;
        }

        if let Some(bool_lit) = expr.as_any().downcast_ref::<Boolean>() {
            let reg = if let Some(d) = into {
                d
            } else {
                self.alloc_register()
            };
            let imm: u16 = if bool_lit.value { 1 } else { 0 };
            self.constant_pool.push(imm as i64);
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
            let index = (self.constant_pool.len() - 1) as u16;
            self.program.push(((index >> 8) & 0xFF) as u8);
            self.program.push((index & 0xFF) as u8);
            return reg;
        }

        if let Some(ident) = expr.as_any().downcast_ref::<Identifier>() {
            if let Some(&reg) = self.symbols.get(&ident.value) {
                // If a destination was requested and differs, we currently
                // don't emit a register-to-register move; just return the
                // existing symbol register.
                return reg;
            }
            // unknown ident -> load 0
            let reg = if let Some(d) = into {
                d
            } else {
                self.alloc_register()
            };
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
            self.program.push(0);
            self.program.push(0);
            return reg;
        }

        if let Some(prefix) = expr.as_any().downcast_ref::<PrefixExpression>() {
            // only support -X by computing 0 - X
            let right_reg = self.compile_expression_dest(&prefix.right, None);
            if prefix.operator == "-" {
                let zero_reg = if let Some(d) = into {
                    d
                } else {
                    self.alloc_register()
                };
                self.program.push(Opcode::LOAD as u8);
                self.program.push(zero_reg);
                self.program.push(0);
                self.program.push(0);

                let dest = if let Some(d) = into {
                    d
                } else {
                    self.alloc_register()
                };
                // SUB zero_reg, right_reg, dest
                self.program.push(Opcode::SUB as u8);
                self.program.push(zero_reg);
                self.program.push(right_reg);
                self.program.push(dest);
                return dest;
            }
            // fallback: return right
            return right_reg;
        }

        if let Some(infix) = expr.as_any().downcast_ref::<InfixExpression>() {
            let start_next = self.next_register;
            let left = self.compile_expression_dest(&infix.left, None);
            let right = self.compile_expression_dest(&infix.right, None);

            let left_is_composite = infix
                .left
                .as_any()
                .downcast_ref::<InfixExpression>()
                .is_some()
                || infix
                    .left
                    .as_any()
                    .downcast_ref::<PrefixExpression>()
                    .is_some();

            let dest = if let Some(d) = into {
                d
            } else if left >= start_next && left_is_composite {
                left
            } else {
                self.alloc_register()
            };

            match infix.operator.as_str() {
                "+" => {
                    println!("COMPILE INFIX '+' left={} right={}", left, right);
                    self.program.push(Opcode::ADD as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "-" => {
                    println!("COMPILE INFIX '-' left={} right={}", left, right);
                    self.program.push(Opcode::SUB as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "*" => {
                    println!("COMPILE INFIX '*' left={} right={}", left, right);
                    self.program.push(Opcode::MUL as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "/" => {
                    println!("COMPILE INFIX '/' left={} right={}", left, right);
                    self.program.push(Opcode::DIV as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "==" | "!=" | ">" | "<" | ">=" | "<=" => {
                    let opcode = match infix.operator.as_str() {
                        "==" => Opcode::EQ,
                        "!=" => Opcode::NEQ,
                        ">" => Opcode::GT,
                        "<" => Opcode::LT,
                        ">=" => Opcode::GTQ,
                        "<=" => Opcode::LTQ,
                        _ => Opcode::IGL,
                    };
                    self.program.push(opcode as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(0);
                    return dest;
                }
                _ => {
                    return left;
                }
            }

            return dest;
        }

        if let Some(if_expr) = expr.as_any().downcast_ref::<IfExpression>() {
            // Compile condition.
            // If the condition is a comparison operation, the comparison
            // opcode already sets VM.equal_flag, so avoid comparing the
            // compiled result to zero (which would overwrite equal_flag).
            let is_comparison = if_expr
                .condition
                .as_any()
                .downcast_ref::<InfixExpression>()
                .map(|inf| matches!(inf.operator.as_str(), "==" | "!=" | ">" | "<" | ">=" | "<="))
                .unwrap_or(false);
            // Placeholder index for the alternative target; will be set inside
            // each branch so we can backpatch after compiling consequence/alt.
            let mut alt_const_idx: usize = 0;

            if is_comparison {
                // compile the comparison which will set equal_flag
                let _ = self.compile_expression_dest(&if_expr.condition, None);

                // reserve placeholder for alternative target and record its index
                self.constant_pool.push(0);
                alt_const_idx = self.constant_pool.len() - 1;

                let alt_reg = self.alloc_register();
                self.program.push(Opcode::LOAD as u8);
                self.program.push(alt_reg);
                let alt_idx_u16 = alt_const_idx as u16;
                self.program.push(((alt_idx_u16 >> 8) & 0xFF) as u8);
                self.program.push((alt_idx_u16 & 0xFF) as u8);

                // For a comparison, equal_flag == true means condition is true.
                // We want to jump to the alternative when condition is false
                // (i.e., equal_flag == false), so emit JNEQ (jump if not equal).
                self.program.push(Opcode::JNEQ as u8);
                self.program.push(alt_reg);
            } else {
                // Non-comparison: evaluate into a register and compare to zero.
                let cond_reg = self.compile_expression_dest(&if_expr.condition, None);

                // load zero constant into a register for comparison
                self.constant_pool.push(0);
                let zero_idx = (self.constant_pool.len() - 1) as u16;
                let zero_reg = self.alloc_register();
                self.program.push(Opcode::LOAD as u8);
                self.program.push(zero_reg);
                self.program.push(((zero_idx >> 8) & 0xFF) as u8);
                self.program.push((zero_idx & 0xFF) as u8);

                // emit EQ cond_reg, zero_reg -> sets equal_flag when condition == 0
                self.program.push(Opcode::EQ as u8);
                self.program.push(cond_reg);
                self.program.push(zero_reg);
                self.program.push(0); // padding

                // prepare placeholder for jump-to-alternative
                self.constant_pool.push(0);
                alt_const_idx = self.constant_pool.len() - 1; // usize
                let alt_reg = self.alloc_register();
                self.program.push(Opcode::LOAD as u8);
                self.program.push(alt_reg);
                let alt_idx_u16 = alt_const_idx as u16;
                self.program.push(((alt_idx_u16 >> 8) & 0xFF) as u8);
                self.program.push((alt_idx_u16 & 0xFF) as u8);

                // JEQ alt_reg -> if condition == 0 jump to alternative start
                self.program.push(Opcode::JEQ as u8);
                self.program.push(alt_reg);
            }

            // result register: if a destination is requested, use it; otherwise allocate
            let dest = if let Some(d) = into {
                d
            } else {
                self.alloc_register()
            };

            // compile consequence: if the last statement is an expression, compile it into dest
            if let Some((last_idx, _)) = if_expr.consequence.statements.iter().enumerate().last() {
                let count = if_expr.consequence.statements.len();
                if count > 0 {
                    for (i, stmt) in if_expr.consequence.statements.iter().enumerate() {
                        if i + 1 == count && let Some(expr_stmt) =
                                stmt.as_any().downcast_ref::<ExpressionStatement>() {
                                let _ =
                                    self.compile_expression_dest(&expr_stmt.expression, Some(dest));
                                continue;
                            }
                        self.compile_statement(stmt);
                    }
                }
            }

            if let Some(alt_block) = &if_expr.alternative {
                // need to jump over the alternative after consequence
                self.constant_pool.push(0);
                let after_const_idx = self.constant_pool.len() - 1;
                let after_reg = self.alloc_register();
                self.program.push(Opcode::LOAD as u8);
                self.program.push(after_reg);
                let after_idx_u16 = (after_const_idx as u16);
                self.program.push(((after_idx_u16 >> 8) & 0xFF) as u8);
                self.program.push((after_idx_u16 & 0xFF) as u8);

                // unconditional jump to after-alternative
                self.program.push(Opcode::JMP as u8);
                self.program.push(after_reg);

                // alternative start position
                let alt_start = self.program.len();
                // If last statement is an expression, compile it into dest
                let count = alt_block.statements.len();
                if count > 0 {
                    for (i, stmt) in alt_block.statements.iter().enumerate() {
                        if i + 1 == count && let Some(expr_stmt) =
                                stmt.as_any().downcast_ref::<ExpressionStatement>() {
                                let _ =
                                    self.compile_expression_dest(&expr_stmt.expression, Some(dest));
                                continue;
                            }
                        self.compile_statement(stmt);
                    }
                }
                let alt_end = self.program.len();

                // backpatch placeholders with actual addresses
                self.constant_pool[alt_const_idx] = alt_start as i64;
                self.constant_pool[after_const_idx] = alt_end as i64;
            } else {
                // no alternative: patch alt to point right after consequence
                let alt_start = self.program.len();
                self.constant_pool[alt_const_idx] = alt_start as i64;
            }

            return dest;
        }

        // CallExpression, IfExpression, FunctionLiteral and others are not implemented yet.
        // Fallback: allocate a register with 0.
        let reg = self.alloc_register();
        self.program.push(Opcode::LOAD as u8);
        self.program.push(reg);
        self.program.push(0);
        self.program.push(0);
        reg
    }
}

/// Convenience function: compile a Program into VM bytes
pub fn compile_program(program: &Program) -> (Vec<u8>, Vec<i64>) {
    // Temporarily use the debug printing version to inspect generated bytecode
    Compiler::new().compile_program(program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Token, TokenType};

    #[test]
    fn test_compile_integer_literal() {
        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token {
                    token_type: TokenType::INT,
                    literal: Some("1".to_string()),
                },
                expression: Box::new(IntegerLiteral {
                    token: Token {
                        token_type: TokenType::INT,
                        literal: Some("1".to_string()),
                    },
                    value: 1,
                }),
            })],
        };

        let (bytes, constant_pool) = compile_program(&program);

        // Expected byte layout:
        // LOAD reg0, 0x0001
        // HLT
        let expected = vec![
            Opcode::LOAD as u8,
            0u8, // register 0
            0u8, // constant pool index high byte
            0u8, // constant pool index low byte
            Opcode::HLT as u8,
        ];

        assert_eq!(bytes, expected);
        assert_eq!(constant_pool, vec![1]);
    }

    #[test]
    fn test_compile_addition() {
        // compile the expression: 1 + 2
        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token {
                    token_type: TokenType::INT,
                    literal: Some("1".to_string()),
                },
                expression: Box::new(InfixExpression {
                    token: Token {
                        token_type: TokenType::PLUS,
                        literal: Some("+".to_string()),
                    },
                    left: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("1".to_string()),
                        },
                        value: 1,
                    }),
                    operator: "+".to_string(),
                    right: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("2".to_string()),
                        },
                        value: 2,
                    }),
                }),
            })],
        };

        let (bytes, pool) = compile_program(&program);

        // Expected byte layout:
        // LOAD reg0, 0x0001
        // LOAD reg1, 0x0002
        // ADD reg0, reg1, reg2
        // HLT
        let expected = vec![
            Opcode::LOAD as u8,
            0u8, // register 0
            0u8, // constant pool index high byte
            0u8, // constant pool index low byte
            Opcode::LOAD as u8,
            1u8, // register 1
            0u8, // constant pool index high byte
            1u8, // constant pool index low byte
            Opcode::ADD as u8,
            0u8, // left operand reg0
            1u8, // right operand reg1
            2u8, // destination reg2
            Opcode::HLT as u8,
        ];

        assert_eq!(pool, vec![1, 2]);
        assert_eq!(bytes, expected);
    }

    #[test]
    fn test_let_then_identifier_uses_same_register() {
        // let x = 1; x;
        let let_stmt = LetStatement {
            token: Token {
                token_type: TokenType::LET,
                literal: Some("let".to_string()),
            },
            name: Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: Some("x".to_string()),
                },
                value: "x".to_string(),
            },
            value: Box::new(IntegerLiteral {
                token: Token {
                    token_type: TokenType::INT,
                    literal: Some("1".to_string()),
                },
                value: 1,
            }),
        };

        let expr_stmt = ExpressionStatement {
            token: Token {
                token_type: TokenType::IDENT,
                literal: Some("x".to_string()),
            },
            expression: Box::new(Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: Some("x".to_string()),
                },
                value: "x".to_string(),
            }),
        };

        let program = Program {
            statements: vec![Box::new(let_stmt), Box::new(expr_stmt)],
        };

        // Only a single LOAD should be emitted for the `let` value, the identifier
        // expression should reuse the register and emit no additional bytes.
        let expected = vec![Opcode::LOAD as u8, 0u8, 0u8, 0u8, Opcode::HLT as u8];

        let bytes = compile_program(&program).0;

        assert_eq!(bytes, expected);
    }

    #[test]
    fn test_compile_if_expression_without_else() {
        // if (true) { 10 }
        let if_expr = IfExpression {
            token: Token {
                token_type: TokenType::IF,
                literal: Some("if".to_string()),
            },
            condition: Box::new(Boolean {
                token: Token {
                    token_type: TokenType::TRUE,
                    literal: Some("true".to_string()),
                },
                value: true,
            }),
            consequence: BlockStatement {
                token: Token {
                    token_type: TokenType::LBRACE,
                    literal: Some("{".to_string()),
                },
                statements: vec![Box::new(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::INT,
                        literal: Some("10".to_string()),
                    },
                    expression: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("10".to_string()),
                        },
                        value: 10,
                    }),
                })],
            },
            alternative: None,
        };

        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token {
                    token_type: TokenType::IF,
                    literal: Some("if".to_string()),
                },
                expression: Box::new(if_expr),
            })],
        };

        let (bytes, pool) = compile_program(&program);

        // Compiler should at least load the constants and emit a conditional jump opcode (JEQ/JNEQ)
        assert!(pool.contains(&10));
        assert!(
            bytes.contains(&(Opcode::JEQ as u8))
                || bytes.contains(&(Opcode::JNEQ as u8))
                || bytes.contains(&(Opcode::JMPF as u8))
        );
    }

    #[test]
    fn test_compile_if_expression_with_else() {
        // if (true) { 10 } else { 20 }
        let if_expr = IfExpression {
            token: Token {
                token_type: TokenType::IF,
                literal: Some("if".to_string()),
            },
            condition: Box::new(Boolean {
                token: Token {
                    token_type: TokenType::TRUE,
                    literal: Some("true".to_string()),
                },
                value: true,
            }),
            consequence: BlockStatement {
                token: Token {
                    token_type: TokenType::LBRACE,
                    literal: Some("{".to_string()),
                },
                statements: vec![Box::new(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::INT,
                        literal: Some("10".to_string()),
                    },
                    expression: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("10".to_string()),
                        },
                        value: 10,
                    }),
                })],
            },
            alternative: Some(BlockStatement {
                token: Token {
                    token_type: TokenType::LBRACE,
                    literal: Some("{".to_string()),
                },
                statements: vec![Box::new(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::INT,
                        literal: Some("20".to_string()),
                    },
                    expression: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("20".to_string()),
                        },
                        value: 20,
                    }),
                })],
            }),
        };

        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token {
                    token_type: TokenType::IF,
                    literal: Some("if".to_string()),
                },
                expression: Box::new(if_expr),
            })],
        };

        let (bytes, pool) = compile_program(&program);

        // Compiler should load both constants and emit conditional and unconditional jumps
        assert!(pool.contains(&10));
        assert!(pool.contains(&20));
        assert!(bytes.contains(&(Opcode::JEQ as u8)) || bytes.contains(&(Opcode::JNEQ as u8)));
        assert!(bytes.contains(&(Opcode::JMP as u8)) || bytes.contains(&(Opcode::JMPF as u8)));
    }

    #[test]
    fn test_compile_if_used_as_expression_in_let() {
        // let x = if (true) { 10 } else { 20 }
        let if_expr = IfExpression {
            token: Token {
                token_type: TokenType::IF,
                literal: Some("if".to_string()),
            },
            condition: Box::new(Boolean {
                token: Token {
                    token_type: TokenType::TRUE,
                    literal: Some("true".to_string()),
                },
                value: true,
            }),
            consequence: BlockStatement {
                token: Token {
                    token_type: TokenType::LBRACE,
                    literal: Some("{".to_string()),
                },
                statements: vec![Box::new(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::INT,
                        literal: Some("10".to_string()),
                    },
                    expression: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("10".to_string()),
                        },
                        value: 10,
                    }),
                })],
            },
            alternative: Some(BlockStatement {
                token: Token {
                    token_type: TokenType::LBRACE,
                    literal: Some("{".to_string()),
                },
                statements: vec![Box::new(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::INT,
                        literal: Some("20".to_string()),
                    },
                    expression: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("20".to_string()),
                        },
                        value: 20,
                    }),
                })],
            }),
        };

        let let_stmt = LetStatement {
            token: Token {
                token_type: TokenType::LET,
                literal: Some("let".to_string()),
            },
            name: Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: Some("x".to_string()),
                },
                value: "x".to_string(),
            },
            value: Box::new(if_expr),
        };

        let program = Program {
            statements: vec![Box::new(let_stmt)],
        };

        let (bytes, pool) = compile_program(&program);

        // Expect both constants to be present and that symbol 'x' was allocated (compiler inserts symbol during let)
        assert!(pool.contains(&10));
        assert!(pool.contains(&20));
        // compiler should emit conditional jump opcode(s)
        assert!(
            bytes.contains(&(Opcode::JEQ as u8))
                || bytes.contains(&(Opcode::JNEQ as u8))
                || bytes.contains(&(Opcode::JMPF as u8))
        );
    }

    #[test]
    fn test_compile_if_used_as_statement_with_let_inside() {
        // if (true) { let x = 10 }
        let let_stmt = LetStatement {
            token: Token {
                token_type: TokenType::LET,
                literal: Some("let".to_string()),
            },
            name: Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: Some("x".to_string()),
                },
                value: "x".to_string(),
            },
            value: Box::new(IntegerLiteral {
                token: Token {
                    token_type: TokenType::INT,
                    literal: Some("10".to_string()),
                },
                value: 10,
            }),
        };

        let if_expr = IfExpression {
            token: Token {
                token_type: TokenType::IF,
                literal: Some("if".to_string()),
            },
            condition: Box::new(Boolean {
                token: Token {
                    token_type: TokenType::TRUE,
                    literal: Some("true".to_string()),
                },
                value: true,
            }),
            consequence: BlockStatement {
                token: Token {
                    token_type: TokenType::LBRACE,
                    literal: Some("{".to_string()),
                },
                statements: vec![Box::new(let_stmt)],
            },
            alternative: None,
        };

        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token {
                    token_type: TokenType::IF,
                    literal: Some("if".to_string()),
                },
                expression: Box::new(if_expr),
            })],
        };

        let (bytes, pool) = compile_program(&program);

        // Expect the constant 10 to be present and that the symbol 'x' was created by compiling the inner let
        assert!(pool.contains(&10));
        // conditional jump emitted
        assert!(
            bytes.contains(&(Opcode::JEQ as u8))
                || bytes.contains(&(Opcode::JNEQ as u8))
                || bytes.contains(&(Opcode::JMPF as u8))
        );
    }
}
