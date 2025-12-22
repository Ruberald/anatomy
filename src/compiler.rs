use std::collections::HashMap;

use crate::ast::*;
use crate::instruction::Opcode;

pub struct Compiler {
    pub program: Vec<u8>,
    next_register: u8,
    symbols: HashMap<String, u8>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            program: vec![],
            next_register: 0,
            symbols: HashMap::new(),
        }
    }

    /// Entry point: compile a Program into raw bytes for the VM.
    pub fn compile_program(mut self, program: &Program) -> Vec<u8> {
        for stmt in &program.statements {
            self.compile_statement(stmt);
        }

        // Ensure program halts when done
        self.program.push(Opcode::HLT as u8);

        self.program
    }

    fn alloc_register(&mut self) -> u8 {
        let r = self.next_register;
        self.next_register = self.next_register.wrapping_add(1);
        r
    }

    fn compile_statement(&mut self, stmt: &Box<dyn Statement>) {
        // Downcast to concrete statements we support
        if let Some(let_stmt) = stmt.as_any().downcast_ref::<LetStatement>() {
            let reg = self.compile_expression(&let_stmt.value);
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

        // other statements: unimplemented
    }

    /// Compile an expression and return the register index holding the result.
    fn compile_expression(&mut self, expr: &Box<dyn Expression>) -> u8 {
        if let Some(int_lit) = expr.as_any().downcast_ref::<IntegerLiteral>() {
            let reg = self.alloc_register();
            // LOAD reg imm16
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
            let value = int_lit.value as u16;
            self.program.push(((value >> 8) & 0xFF) as u8);
            self.program.push((value & 0xFF) as u8);
            return reg;
        }

        if let Some(bool_lit) = expr.as_any().downcast_ref::<Boolean>() {
            let reg = self.alloc_register();
            let imm: u16 = if bool_lit.value { 1 } else { 0 };
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
            self.program.push(((imm >> 8) & 0xFF) as u8);
            self.program.push((imm & 0xFF) as u8);
            return reg;
        }

        if let Some(ident) = expr.as_any().downcast_ref::<Identifier>() {
            if let Some(&reg) = self.symbols.get(&ident.value) {
                return reg;
            }
            // unknown ident -> load 0
            let reg = self.alloc_register();
            self.program.push(Opcode::LOAD as u8);
            self.program.push(reg);
            self.program.push(0);
            self.program.push(0);
            return reg;
        }

        if let Some(prefix) = expr.as_any().downcast_ref::<PrefixExpression>() {
            // only support -X by computing 0 - X
            let right_reg = self.compile_expression(&prefix.right);
            if prefix.operator == "-" {
                let zero_reg = self.alloc_register();
                self.program.push(Opcode::LOAD as u8);
                self.program.push(zero_reg);
                self.program.push(0);
                self.program.push(0);

                let dest = self.alloc_register();
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
            let left = self.compile_expression(&infix.left);
            let right = self.compile_expression(&infix.right);
            let dest = self.alloc_register();

            match infix.operator.as_str() {
                "+" => {
                    self.program.push(Opcode::ADD as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "-" => {
                    self.program.push(Opcode::SUB as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "*" => {
                    self.program.push(Opcode::MUL as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "/" => {
                    self.program.push(Opcode::DIV as u8);
                    self.program.push(left);
                    self.program.push(right);
                    self.program.push(dest);
                }
                "==" | "!=" | ">" | "<" | ">=" | "<=" => {
                    // The VM currently exposes flags but not boolean-result-into-register.
                    // We can set the equal_flag using the appropriate opcode, but we don't
                    // have a move-from-flag instruction. For now, compile comparisons by
                    // emitting the comparison opcode (which sets flags) and leave dest=0.
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
                    // opcode implementations read an extra byte after two operands; add padding
                    self.program.push(0);
                    // return a register containing 0 (no reliable boolean value as register)
                    return dest;
                }
                _ => {
                    // unsupported operator: no-op, return left
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
pub fn compile_program(program: &Program) -> Vec<u8> {
    Compiler::new().compile_program(program)
}
