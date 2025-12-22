use crate::instruction::Opcode;

pub struct VM {
    pub registers: [i32; 32],
    pc: usize,
    pub program: Vec<u8>,
    remainder: u32,
    equal_flag: bool,
}

impl VM {
    pub fn new() -> Self {
        VM {
            registers: [0; 32],
            pc: 0,
            program: vec![],
            remainder: 0,
            equal_flag: false,
        }
    }

    /// Replace the VM's program and reset the program counter.
    pub fn load_program(&mut self, bytes: Vec<u8>) {
        self.program = bytes;
        self.pc = 0;
        self.remainder = 0;
        self.equal_flag = false;
    }

    fn decode_opcode(&mut self) -> Opcode {
        let opcode = Opcode::from(self.program[self.pc]);
        self.pc += 1;
        // println!("{:?}", opcode);
        opcode
    }

    fn next_8_bits(&mut self) -> u8 {
        let result = self.program[self.pc];
        self.pc += 1;
        result
    }

    fn next_16_bits(&mut self) -> u16 {
        let result = ((self.program[self.pc] as u16) << 8) | self.program[self.pc + 1] as u16;
        self.pc += 2;
        result
    }

    /// Loops as long as instructions can be executed.
    pub fn run(&mut self) {
        let mut is_done = false;
        while !is_done {
            is_done = self.execute_instruction();
        }
    }

    /// Executes one instruction. Meant to allow for more controlled execution of the VM
    pub fn run_once(&mut self) {
        self.execute_instruction();
    }

    fn execute_instruction(&mut self) -> bool {
        if self.pc >= self.program.len() {
            return true;
        }

        match self.decode_opcode() {
            Opcode::LOAD => {
                let register = self.next_8_bits() as usize;
                let number = self.next_16_bits() as u32;
                println!("Loading {number} into register {register}");
                self.registers[register] = number as i32;
            }

            Opcode::HLT => {
                println!("HLT encountered");
                return true;
            }

            Opcode::ADD => {
                let register1 = self.registers[self.next_8_bits() as usize];
                let register2 = self.registers[self.next_8_bits() as usize];
                self.registers[self.next_8_bits() as usize] = register1 + register2;
            }

            Opcode::SUB => {
                let register1 = self.registers[self.next_8_bits() as usize];
                let register2 = self.registers[self.next_8_bits() as usize];
                self.registers[self.next_8_bits() as usize] = register1 - register2;
            }

            Opcode::MUL => {
                let register1 = self.registers[self.next_8_bits() as usize];
                let register2 = self.registers[self.next_8_bits() as usize];
                self.registers[self.next_8_bits() as usize] = register1 * register2;
            }

            Opcode::DIV => {
                let register1 = self.registers[self.next_8_bits() as usize];
                let register2 = self.registers[self.next_8_bits() as usize];
                self.registers[self.next_8_bits() as usize] = register1 / register2;
                self.remainder = (register1 % register2) as u32;
            }

            Opcode::JMP => {
                self.pc = self.registers[self.next_8_bits() as usize] as usize;
            }

            Opcode::JMPF => {
                self.pc += self.registers[self.next_8_bits() as usize] as usize;
            }

            Opcode::JMPB => {
                self.pc -= self.registers[self.next_8_bits() as usize] as usize;
            }

            Opcode::EQ => {
                let op1 = self.registers[self.next_8_bits() as usize];
                let op2 = self.registers[self.next_8_bits() as usize];

                println!("{op1} == {op2}");

                self.equal_flag = op1 == op2;

                self.next_8_bits();
            }

            Opcode::NEQ => {
                let op1 = self.registers[self.next_8_bits() as usize];
                let op2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = op1 != op2;

                self.next_8_bits();
            }

            Opcode::GT => {
                let op1 = self.registers[self.next_8_bits() as usize];
                let op2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = op1 > op2;

                self.next_8_bits();
            }

            Opcode::GTQ => {
                let op1 = self.registers[self.next_8_bits() as usize];
                let op2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = op1 >= op2;

                self.next_8_bits();
            }

            Opcode::LT => {
                let op1 = self.registers[self.next_8_bits() as usize];
                let op2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = op1 < op2;

                self.next_8_bits();
            }

            Opcode::LTQ => {
                let op1 = self.registers[self.next_8_bits() as usize];
                let op2 = self.registers[self.next_8_bits() as usize];

                self.equal_flag = op1 <= op2;

                self.next_8_bits();
            }

            Opcode::JEQ => {
                let target = self.registers[self.next_8_bits() as usize] as usize;
                if self.equal_flag {
                    self.pc = target;
                }
            }

            Opcode::JNEQ => {
                let target = self.registers[self.next_8_bits() as usize] as usize;
                if !self.equal_flag {
                    self.pc = target;
                }
            }

            Opcode::IGL => {
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_vm() {
        let test_vm = VM::new();
        assert_eq!(test_vm.registers[0], 0)
    }

    #[test]
    fn test_opcode_hlt() {
        let mut test_vm = VM::new();
        let test_bytes = vec![5, 0, 0, 0];
        test_vm.program = test_bytes;
        test_vm.run_once();
        assert_eq!(test_vm.pc, 1);
    }

    #[test]
    fn test_opcode_igl() {
        let mut test_vm = VM::new();
        let test_bytes = vec![200, 0, 0, 0];
        test_vm.program = test_bytes;
        test_vm.run();
        assert_eq!(test_vm.pc, 1);
    }

    #[test]
    fn test_jmp_opcode() {
        let mut test_vm = VM::new();
        test_vm.registers[0] = 1;
        test_vm.program = vec![6, 0, 0, 0];
        test_vm.run_once();
        assert_eq!(test_vm.pc, 1);
    }

    #[test]
    fn test_eq_opcode() {
        let mut test_vm = VM::new();
        test_vm.registers[0] = 10;
        test_vm.registers[1] = 10;
        test_vm.program = vec![9, 0, 1, 0, 9, 0, 1, 0];
        test_vm.run_once();
        assert!(test_vm.equal_flag);
        test_vm.registers[1] = 20;
        test_vm.run_once();
        assert!(!test_vm.equal_flag);
    }
}
