

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Value {
    key: u64
}

impl Value {
    pub fn new(key: u64) -> Value {
        Value { key }
    }
}

pub enum Instruction {
    LoadI32(Value, i32),
    Return(Value)
}

impl Instruction {
    pub fn written(&self) -> Option<Value> {
        match self {
            Instruction::LoadI32(value, _) => Some(*value),
            Instruction::Return(_) => None
        }
    }

    pub fn used(&self) -> Vec<Value> {
        match self {
            Instruction::LoadI32(_, _) => vec![],
            Instruction::Return(value) => vec![*value]
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct BlockId {
    pub key: u64
}

impl BlockId {
    pub fn new(key: u64) -> BlockId { BlockId { key } }
}

pub struct Block {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
}