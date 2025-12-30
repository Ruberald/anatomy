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
    pub fn compile_statements(&mut self, program: &Program) -> (Vec<u8>, Vec<i64>, Option<u8>) {
        self.program.clear();

        let mut last_expr_reg: Option<u8> = None;
        for stmt in &program.statements {
            let maybe = self.compile_statement(stmt);
            if let Some(r) = maybe {
                last_expr_reg = Some(r);
            }
        }

        // Ensure program halts when done
        self.program.push(Opcode::HLT as u8);

        log::debug!("next_register: {}", self.next_register);
        log::debug!("symbols: {:?}", self.symbols);
        log::debug!("bytes: {:?}", self.program);
        log::debug!("constant_pool: {:?}", self.constant_pool);

        (
            self.program.clone(),
            self.constant_pool.clone(),
            last_expr_reg,
        )
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

    /// Intern a constant value into the constant pool and return its index.
    /// This deduplicates constants so the same value isn't stored multiple times.
    fn intern_constant(&mut self, val: i64) -> u16 {
        // simple linear search for now (constant pool is small); return existing index if found
        for (i, v) in self.constant_pool.iter().enumerate() {
            if *v == val {
                return i as u16;
            }
        }
        self.constant_pool.push(val);
        (self.constant_pool.len() - 1) as u16
    }

    /// Reserve a slot in the constant pool for backpatching and return its index.
    fn reserve_constant(&mut self) -> usize {
        self.constant_pool.push(0);
        self.constant_pool.len() - 1
    }

    /// Return the register index for a named symbol, if present.
    pub fn get_symbol(&self, name: &str) -> Option<u8> {
        self.symbols.get(name).copied()
    }

    fn compile_statement(&mut self, stmt: &Box<dyn Statement>) -> Option<u8> {
        // Downcast to concrete statements we support
        if let Some(let_stmt) = stmt.as_any().downcast_ref::<LetStatement>() {
            // Allocate a register for the `let` binding and compile the value into it.
            let reg = self.alloc_register();
            let _ = self.compile_expression_dest(&let_stmt.value, Some(reg));
            // Do not overwrite an existing symbol mapping; keep the first
            // binding encountered for a given name so later conditional
            // branches don't clobber earlier runtime-visible bindings.
            self.symbols
                .entry(let_stmt.name.value.clone())
                .or_insert(reg);
            return None;
        }

        if let Some(expr_stmt) = stmt.as_any().downcast_ref::<ExpressionStatement>() {
            // compile expression and return the register holding the result
            let reg = self.compile_expression(&expr_stmt.expression);
            return Some(reg);
        }

        if let Some(_ret) = stmt.as_any().downcast_ref::<ReturnStatement>() {
            // No call/return support in the current VM; ignore or HLT
            self.program.push(Opcode::HLT as u8);
            return None;
        }

        if let Some(_block) = stmt.as_any().downcast_ref::<BlockStatement>() {
            // Not implemented yet; no-op
        }
        None
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
            let index = self.intern_constant(int_lit.value);
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
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
            let index = self.intern_constant(imm as i64);
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
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
            let zero_idx = self.intern_constant(0);
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
            self.program.push(((zero_idx >> 8) & 0xFF) as u8);
            self.program.push((zero_idx & 0xFF) as u8);
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
                let zero_idx = self.intern_constant(0);
                self.program.push(Opcode::LOAD as u8);
                self.program.push(zero_reg);
                self.program.push(((zero_idx >> 8) & 0xFF) as u8);
                self.program.push((zero_idx & 0xFF) as u8);

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
                    log::debug!("COMPILE INFIX '+' left={} right={}", left, right);
                    self.program.push(Opcode::ADD as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "-" => {
                    log::debug!("COMPILE INFIX '-' left={} right={}", left, right);
                    self.program.push(Opcode::SUB as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "*" => {
                    log::debug!("COMPILE INFIX '*' left={} right={}", left, right);
                    self.program.push(Opcode::MUL as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "/" => {
                    log::debug!("COMPILE INFIX '/' left={} right={}", left, right);
                    self.program.push(Opcode::DIV as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "=" => {
                    // Assignment: compile RHS into the LHS symbol's register.
                    if let Some(ident) =
                        infix.left.as_any().downcast_ref::<crate::ast::Identifier>()
                    {
                        let name = ident.value.clone();
                        if let Some(&reg) = self.symbols.get(&name) {
                            let _ = self.compile_expression_dest(&infix.right, Some(reg));
                            return reg;
                        } else {
                            // If symbol didn't exist, create it (like a top-level let)
                            let reg = self.alloc_register();
                            let _ = self.compile_expression_dest(&infix.right, Some(reg));
                            self.symbols.insert(name, reg);
                            return reg;
                        }
                    }
                    // fallback: compile right and return its register
                    let r = self.compile_expression_dest(&infix.right, None);
                    return r;
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
                alt_const_idx = self.reserve_constant();

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
                let zero_idx = self.intern_constant(0) as u16;
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
                alt_const_idx = self.reserve_constant(); // usize
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

            // Determine if the condition is a compile-time boolean literal.
            // If it's known (true/false) we may commit symbols created in the
            // corresponding branch into the parent symbol table. If unknown,
            // compile branches in an isolated symbol table so branch-local
            // `let` bindings don't leak into the outer scope.
            let const_condition = if_expr
                .condition
                .as_any()
                .downcast_ref::<Boolean>()
                .map(|b| b.value);

            // We'll use a helper method `compile_block` (defined below) to
            // compile blocks with controlled symbol scoping. This avoids
            // borrowing `self` mutably inside a closure here which caused
            // borrow-checker conflicts.

            // Compile consequence. If the condition is a compile-time `true`,
            // commit symbol changes from the consequence; otherwise isolate them.
            let consequence_commit = matches!(const_condition, Some(true));
            if let Some(_) = if_expr
                .consequence
                .statements
                .iter()
                .enumerate()
                .next_back()
            {
                self.compile_block(&if_expr.consequence, consequence_commit, dest);
            }

            if let Some(alt_block) = &if_expr.alternative {
                // need to jump over the alternative after consequence
                let after_const_idx = self.reserve_constant();
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
                // If the condition is a compile-time `false`, commit the
                // alternative's symbol changes to the parent; otherwise
                // isolate them so branch-local lets don't leak.
                let alternative_commit = matches!(const_condition, Some(false));
                self.compile_block(alt_block, alternative_commit, dest);
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
        let zero_idx = self.intern_constant(0);
        self.program.push(((zero_idx >> 8) & 0xFF) as u8);
        self.program.push((zero_idx & 0xFF) as u8);
        reg
    }

    fn compile_block(&mut self, block: &BlockStatement, commit: bool, dest: u8) {
        if commit {
            let count = block.statements.len();
            if count > 0 {
                for (i, stmt) in block.statements.iter().enumerate() {
                    if i + 1 == count {
                        if let Some(expr_stmt) = stmt.as_any().downcast_ref::<ExpressionStatement>()
                        {
                            let _ = self.compile_expression_dest(&expr_stmt.expression, Some(dest));
                            continue;
                        }
                    }
                    self.compile_statement(stmt);
                }
            }
        } else {
            let saved_symbols = self.symbols.clone();
            // Use a copy so lookups for outer symbols still succeed
            self.symbols = saved_symbols.clone();
            let count = block.statements.len();
            if count > 0 {
                for (i, stmt) in block.statements.iter().enumerate() {
                    if i + 1 == count {
                        if let Some(expr_stmt) = stmt.as_any().downcast_ref::<ExpressionStatement>()
                        {
                            let _ = self.compile_expression_dest(&expr_stmt.expression, Some(dest));
                            continue;
                        }
                    }
                    self.compile_statement(stmt);
                }
            }
            // discard symbol changes from the branch
            self.symbols = saved_symbols;
        }
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

    #[test]
    fn test_constant_pool_deduplicates_integer_literals() {
        // two top-level integer literals should produce a single constant pool entry
        let program = Program {
            statements: vec![
                Box::new(ExpressionStatement {
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
                }),
                Box::new(ExpressionStatement {
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
                }),
            ],
        };

        let (_bytes, pool) = compile_program(&program);
        // only a single '1' should be stored
        assert_eq!(pool, vec![1]);
    }

    #[test]
    fn test_constant_pool_deduplicates_zero() {
        // ensure multiple zeros are deduplicated
        let program = Program {
            statements: vec![
                Box::new(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::INT,
                        literal: Some("0".to_string()),
                    },
                    expression: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("0".to_string()),
                        },
                        value: 0,
                    }),
                }),
                Box::new(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::INT,
                        literal: Some("0".to_string()),
                    },
                    expression: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("0".to_string()),
                        },
                        value: 0,
                    }),
                }),
            ],
        };

        let (_bytes, pool) = compile_program(&program);
        assert_eq!(pool, vec![0]);
    }

    #[test]
    fn test_assignment_to_non_identifier_does_not_crash() {
        // (1 + 2) = 5; -> left is a composite expression; compiler should
        // compile the RHS and not panic. We don't assert specific bytes,
        // just ensure compile_program returns without panicking and produces
        // a non-empty program and a constant pool that contains 5.
        let infix_left = InfixExpression {
            token: Token {
                token_type: TokenType::LPAREN,
                literal: Some("(".to_string()),
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
        };

        let program = Program {
            statements: vec![Box::new(ExpressionStatement {
                token: Token {
                    token_type: TokenType::EQ,
                    literal: Some("=".to_string()),
                },
                expression: Box::new(InfixExpression {
                    token: Token {
                        token_type: TokenType::EQ,
                        literal: Some("=".to_string()),
                    },
                    left: Box::new(infix_left),
                    operator: "=".to_string(),
                    right: Box::new(IntegerLiteral {
                        token: Token {
                            token_type: TokenType::INT,
                            literal: Some("5".to_string()),
                        },
                        value: 5,
                    }),
                }),
            })],
        };

        let (bytes, pool) = compile_program(&program);
        assert!(!bytes.is_empty(), "compiled bytes should not be empty");
        assert!(pool.contains(&5), "constant pool should contain 5");
    }

    #[test]
    fn test_compile_assignment_runtime() {
        // let x = 10; x = 20;
        let input = "let x = 10; x = 20;";
        let mut lexer = crate::lexer::Lexer::new(input.to_string());
        let mut parser = crate::parser::Parser::new(lexer);
        let program = parser.parse_program();
        crate::parser::check_parser_errors(&parser);

        let mut compiler = Compiler::new();
        let (bytes, pool, _last) = compiler.compile_statements(&program);

        let mut vm = crate::vm::VM::new(bytes, pool);
        vm.run();

        let reg = compiler.get_symbol("x").expect("symbol x should exist");
        assert_eq!(vm.frames[0].registers[reg as usize], 20);
    }
}
