use std::collections::HashMap;

use crate::ast::*;
use crate::instruction::Opcode;

pub struct Compiler {
    pub program: Vec<u8>,
    next_register: u8,
    symbols: HashMap<String, u8>,
    pub constant_pool: Vec<i64>,
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
}
