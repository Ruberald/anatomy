#[derive(Debug, PartialEq)]
pub enum Opcode {
    LOAD,
    ADD,
    SUB,
    MUL,
    DIV,
    HLT,
    JMP,
    JMPF,
    JMPB,
    EQ,
    NEQ,
    GT,
    LT,
    GTQ,
    LTQ,
    JEQ,
    JNEQ,
    IGL,
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
    opcode: Opcode,
}

impl Instruction {
    pub fn new(opcode: Opcode) -> Self {
        Instruction { opcode }
    }
}

impl From<u8> for Opcode {
    fn from(v: u8) -> Opcode {
        match v {
            0 => Self::LOAD,
            1 => Self::ADD,
            2 => Self::SUB,
            3 => Self::MUL,
            4 => Self::DIV,
            5 => Self::HLT,
            6 => Self::JMP,
            7 => Self::JMPF,
            8 => Self::JMPB,
            9 => Self::EQ,
            10 => Self::NEQ,
            11 => Self::GT,
            12 => Self::LT,
            13 => Self::GTQ,
            14 => Self::LTQ,
            15 => Self::JEQ,
            16 => Self::JNEQ,
            _ => Self::IGL
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_hlt() {
        let opcode = Opcode::HLT;
        assert_eq!(opcode, Opcode::HLT);
    }

    #[test]
    fn test_create_instruction() {
        let instruction = Instruction::new(Opcode::HLT);
        assert_eq!(instruction.opcode, Opcode::HLT);
    }
}

