#[derive(Debug, PartialEq)]
pub enum Opcode {
  HLT,
  IGL
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
    opcode: Opcode,
}

impl Instruction {
    pub fn new() -> Self {

    }
}
