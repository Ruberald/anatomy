use crate::instruction::Opcode;

pub struct CallFrame {
    pub pc: usize,
    pub registers: [i64; 32],
}

pub struct VM {
    pub frames: Vec<CallFrame>,
    pub program: Vec<u8>,
    remainder: u32,
    pub equal_flag: bool,
    pub constant_pool: Vec<i64>,
}

impl VM {
    pub fn new(bytes: Vec<u8>, constant_pool: Vec<i64>) -> Self {
        VM {
            frames: vec![CallFrame {
                pc: 0,
                registers: [0; 32],
            }],
            program: bytes,
            remainder: 0,
            equal_flag: false,
            constant_pool,
        }
    }

    fn decode_opcode(&mut self) -> Opcode {
        let frame = self.frames.last_mut().unwrap();
        let opcode = Opcode::from(self.program[frame.pc]);
        frame.pc += 1;
        // println!("{:?}", opcode);
        opcode
    }

    fn next_8_bits(&mut self) -> u8 {
        let frame = self.frames.last_mut().unwrap();
        let result = self.program[frame.pc];
        frame.pc += 1;
        result
    }

    fn next_16_bits(&mut self) -> u16 {
        let frame = self.frames.last_mut().unwrap();
        let result = ((self.program[frame.pc] as u16) << 8) | self.program[frame.pc + 1] as u16;
        frame.pc += 2;
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
        if self.frames.last().unwrap().pc >= self.program.len() {
            return true;
        }

        match self.decode_opcode() {
            Opcode::LOAD => {
                let register = self.next_8_bits() as usize;
                let index = self.next_16_bits() as usize;
                let number = self.constant_pool[index];
                log::debug!("Loading {number} into register {register}");
                let frame = self.frames.last_mut().unwrap();
                frame.registers[register] = number;
            }

            Opcode::HLT => {
                log::debug!("HLT encountered");
                return true;
            }

            Opcode::ADD => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let dest = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                frame.registers[dest] = frame.registers[r1] + frame.registers[r2];
            }

            Opcode::SUB => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let dest = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                frame.registers[dest] = frame.registers[r1] - frame.registers[r2];
            }

            Opcode::MUL => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let dest = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                frame.registers[dest] = frame.registers[r1] * frame.registers[r2];
            }

            Opcode::DIV => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let dest = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                frame.registers[dest] = frame.registers[r1] / frame.registers[r2];
                self.remainder = (frame.registers[r1] % frame.registers[r2]) as u32;
            }

            Opcode::JMP => {
                let reg_idx = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                frame.pc = frame.registers[reg_idx] as usize;
            }

            Opcode::JMPF => {
                let reg_idx = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                frame.pc += frame.registers[reg_idx] as usize;
            }

            Opcode::JMPB => {
                let reg_idx = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                frame.pc -= frame.registers[reg_idx] as usize;
            }

            Opcode::EQ => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                // padding byte (VM design quirk)
                let _ = self.next_8_bits();
                let frame = self.frames.last_mut().unwrap();
                let op1 = frame.registers[r1];
                let op2 = frame.registers[r2];

                log::debug!("{op1} == {op2}");

                self.equal_flag = op1 == op2;
            }

            Opcode::NEQ => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let _ = self.next_8_bits();
                let frame = self.frames.last_mut().unwrap();
                let op1 = frame.registers[r1];
                let op2 = frame.registers[r2];

                self.equal_flag = op1 != op2;
            }

            Opcode::GT => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let _ = self.next_8_bits();
                let frame = self.frames.last_mut().unwrap();
                let op1 = frame.registers[r1];
                let op2 = frame.registers[r2];

                self.equal_flag = op1 > op2;
            }

            Opcode::GTQ => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let _ = self.next_8_bits();
                let frame = self.frames.last_mut().unwrap();
                let op1 = frame.registers[r1];
                let op2 = frame.registers[r2];

                self.equal_flag = op1 >= op2;
            }

            Opcode::LT => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let _ = self.next_8_bits();
                let frame = self.frames.last_mut().unwrap();
                let op1 = frame.registers[r1];
                let op2 = frame.registers[r2];

                self.equal_flag = op1 < op2;
            }

            Opcode::LTQ => {
                let r1 = self.next_8_bits() as usize;
                let r2 = self.next_8_bits() as usize;
                let _ = self.next_8_bits();
                let frame = self.frames.last_mut().unwrap();
                let op1 = frame.registers[r1];
                let op2 = frame.registers[r2];

                self.equal_flag = op1 <= op2;
            }

            Opcode::JEQ => {
                let reg_idx = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                let target = frame.registers[reg_idx] as usize;
                if self.equal_flag {
                    frame.pc = target;
                }
            }

            Opcode::JNEQ => {
                let reg_idx = self.next_8_bits() as usize;
                let frame = self.frames.last_mut().unwrap();
                let target = frame.registers[reg_idx] as usize;
                if !self.equal_flag {
                    frame.pc = target;
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
        let test_vm = VM::new(vec![], vec![]);
        assert_eq!(test_vm.frames[0].registers[0], 0)
    }

    #[test]
    fn test_opcode_hlt() {
        let mut test_vm = VM::new(vec![], vec![]);
        let test_bytes = vec![5, 0, 0, 0];
        test_vm.program = test_bytes;
        test_vm.run_once();
        assert_eq!(test_vm.frames[0].pc, 1);
    }

    #[test]
    fn test_opcode_igl() {
        let test_bytes = vec![200, 0, 0, 0];
        let mut test_vm = VM::new(test_bytes.clone(), vec![]);
        test_vm.program = test_bytes;
        test_vm.run();
        assert_eq!(test_vm.frames[0].pc, 1);
    }

    #[test]
    fn test_jmp_opcode() {
        let mut test_vm = VM::new(vec![], vec![]);
        test_vm.frames[0].registers[0] = 1;
        test_vm.program = vec![6, 0, 0, 0];
        test_vm.run_once();
        assert_eq!(test_vm.frames[0].pc, 1);
    }

    #[test]
    fn test_eq_opcode() {
        let mut test_vm = VM::new(vec![], vec![]);
        test_vm.frames[0].registers[0] = 10;
        test_vm.frames[0].registers[1] = 10;
        test_vm.program = vec![9, 0, 1, 0, 9, 0, 1, 0];
        test_vm.run_once();
        assert!(test_vm.equal_flag);
        test_vm.frames[0].registers[1] = 20;
        test_vm.run_once();
        assert!(!test_vm.equal_flag);
    }
}
