use super::{Cpu, Register};

#[derive(Debug, Copy, Clone)]
pub(super) struct InstructionInfo {
    pub inst: Instruction,
    pub cycles_remaining: usize,
    pub address: u16,
}

impl Default for InstructionInfo {
    fn default() -> Self {
        InstructionInfo {
            inst: Instruction::Initial,
            cycles_remaining: 0,
            address: 0,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub(super) enum Instruction {
    Initial,
    SetInterruptDisable,
    ClearDecimal,
    ClearCarry,
    LoadAccumulator(AddressingMode),
    StoreAccumulator(AddressingMode),
    PushAccumulator,
    PopAccumulator,
    LoadX(AddressingMode),
    StoreX(AddressingMode),
    // src, dest
    RegisterTransfer(Register, Register),
    LoadY(AddressingMode),
    StoreY(AddressingMode),
    DecrementX,
    DecrementY,
    IncrementX,
    IncrementY,
    IncrementMemory(AddressingMode),
    DecrementMemory(AddressingMode),
    And(AddressingMode),
    Branch(BranchType),
    JumpSavingReturn,
    ReturnFromSubroutine,
    Jump(AddressingMode),
    CompareWithAccumulator(AddressingMode),
    CompareWithX(AddressingMode),
    CompareWithY(AddressingMode),
    ShiftAccumulatorRight,
    ShiftAccumulatorLeft,
    ShiftRight(AddressingMode),
    ShiftLeft(AddressingMode),
    AddToAccumulatorWithCarry(AddressingMode),
    ExclusiveOr(AddressingMode),
    RotateAccumulatorRight,
    RotateRight(AddressingMode),
    TestBits(AddressingMode),
    NoOp,
}

#[derive(Debug, Copy, Clone)]
pub(super) enum BranchType {
    ResultZero,
    ResultNotZero,
    ResultPlus,
    ResultMinus,
    CarryClear,
    CarrySet,
}

#[derive(Debug, Copy, Clone)]
pub(super) enum AddressingMode {
    ZeroPage,
    IndexedZeroPageX,
    IndexedZeroPageY,
    Absolute,
    IndexedAbsoluteX,
    IndexedAbsoluteY,
    Indirect,
    Immediate,
    IndexedIndirect,
    IndirectIndexed,
}

impl AddressingMode {
    /// Returns the target address for the operation based on mode. Mutates the passed CPU to update the
    /// program counter for consumed operands.
    pub(super) fn get_target_addr_and_adjust_pc(&self, cpu: &mut Cpu) -> u16 {
        use AddressingMode::*;

        match *self {
            ZeroPage => {
                let target_addr = cpu.memory.read_u8(cpu.pc) as u16;
                cpu.pc += 1;
                target_addr
            }
            IndexedZeroPageX => {
                let target_addr = cpu.memory.read_u8(cpu.pc).wrapping_add(cpu.x) as u16;
                cpu.pc += 1;
                target_addr
            }
            IndexedZeroPageY => {
                let target_addr = cpu.memory.read_u8(cpu.pc).wrapping_add(cpu.y) as u16;
                cpu.pc += 1;
                target_addr
            }
            Absolute => {
                let target_addr = cpu.memory.read_u16(cpu.pc);
                cpu.pc += 2;
                target_addr
            }
            IndexedAbsoluteX => {
                let target_addr = cpu.memory.read_u16(cpu.pc).wrapping_add(cpu.x as u16);
                cpu.pc += 2;
                target_addr
            }
            IndexedAbsoluteY => {
                let target_addr = cpu.memory.read_u16(cpu.pc).wrapping_add(cpu.y as u16);
                cpu.pc += 2;
                target_addr
            }
            Indirect => {
                let indirect_addr = cpu.memory.read_u16(cpu.pc);
                cpu.pc += 2;
                cpu.memory.read_u16(indirect_addr)
            }
            IndexedIndirect => {
                let indirect_addr = cpu.memory.read_u8(cpu.pc).wrapping_add(cpu.x) as u16;
                cpu.pc += 1;
                cpu.memory.read_u16(indirect_addr)
            }
            IndirectIndexed => {
                let indirect_base = cpu.memory.read_u8(cpu.pc) as u16;
                cpu.pc += 1;
                cpu.memory
                    .read_u16(indirect_base)
                    .wrapping_add(cpu.y as u16)
            }
            Immediate => unimplemented!(),
        }
    }

    /// Returns the value for the operation based on mode. Mutates the passed CPU to update the
    /// program counter for consumed operands.
    pub(super) fn get_value_and_adjust_pc(&self, cpu: &mut Cpu) -> u8 {
        use AddressingMode::*;

        match *self {
            ZeroPage => {
                let target_addr = cpu.memory.read_u8(cpu.pc) as u16;
                cpu.pc += 1;
                cpu.memory.read_u8(target_addr)
            }
            IndexedZeroPageX => {
                let target_addr = cpu.memory.read_u8(cpu.pc).wrapping_add(cpu.x) as u16;
                cpu.pc += 1;
                cpu.memory.read_u8(target_addr)
            }
            IndexedZeroPageY => {
                let target_addr = cpu.memory.read_u8(cpu.pc).wrapping_add(cpu.y) as u16;
                cpu.pc += 1;
                cpu.memory.read_u8(target_addr)
            }
            Absolute => {
                let target_addr = cpu.memory.read_u16(cpu.pc);
                cpu.pc += 2;
                cpu.memory.read_u8(target_addr)
            }
            IndexedAbsoluteX => {
                let target_addr = cpu.memory.read_u16(cpu.pc).wrapping_add(cpu.x as u16);
                cpu.pc += 2;
                cpu.memory.read_u8(target_addr)
            }
            IndexedAbsoluteY => {
                let target_addr = cpu.memory.read_u16(cpu.pc).wrapping_add(cpu.y as u16);
                cpu.pc += 2;
                cpu.memory.read_u8(target_addr)
            }
            Indirect => {
                let indirect_addr = cpu.memory.read_u16(cpu.pc);
                cpu.pc += 2;
                let target_addr = cpu.memory.read_u16(indirect_addr);
                cpu.memory.read_u8(target_addr)
            }
            IndexedIndirect => {
                let indirect_addr = cpu.memory.read_u8(cpu.pc).wrapping_add(cpu.x) as u16;
                cpu.pc += 1;
                let target_addr = cpu.memory.read_u16(indirect_addr);
                cpu.memory.read_u8(target_addr)
            }
            IndirectIndexed => {
                let indirect_base = cpu.memory.read_u8(cpu.pc) as u16;
                cpu.pc += 1;
                let target_addr = cpu
                    .memory
                    .read_u16(indirect_base)
                    .wrapping_add(cpu.y as u16);
                cpu.memory.read_u8(target_addr)
            }
            Immediate => {
                let v = cpu.memory.read_u8(cpu.pc);
                cpu.pc += 1;
                v
            }
        }
    }
}

pub(super) fn get_instruction(cpu: &Cpu) -> InstructionInfo {
    use AddressingMode::*;
    use BranchType::*;
    use Instruction::*;
    use Register::*;

    let opcode = cpu.memory.read_u8(cpu.pc);

    //TODO: handle cycle differences depending on which page the jump targets - will probably need to do some lookahead here and store details in the info struct
    let (inst, cycles) = match opcode {
        0x06 => (ShiftLeft(ZeroPage), 5),
        0x0a => (ShiftAccumulatorLeft, 2),
        0x0e => (ShiftLeft(Absolute), 6),
        0x10 => (Branch(ResultPlus), 2),
        0x16 => (ShiftLeft(IndexedZeroPageX), 6),
        0x18 => (ClearCarry, 2),
        0x1e => (ShiftLeft(IndexedAbsoluteX), 7),
        0x20 => (JumpSavingReturn, 6),
        0x21 => (And(IndexedIndirect), 6),
        0x24 => (TestBits(ZeroPage), 3),
        0x25 => (And(ZeroPage), 3),
        0x29 => (And(Immediate), 2),
        0x2c => (TestBits(Absolute), 4),
        0x2d => (And(Absolute), 4),
        0x30 => (Branch(ResultMinus), 2),
        0x31 => (And(IndirectIndexed), 5),
        0x35 => (And(IndexedZeroPageX), 4),
        0x39 => (And(IndexedAbsoluteY), 4),
        0x3d => (And(IndexedAbsoluteX), 4),
        0x41 => (ExclusiveOr(IndexedIndirect), 6),
        0x45 => (ExclusiveOr(ZeroPage), 3),
        0x46 => (ShiftRight(ZeroPage), 5),
        0x48 => (PushAccumulator, 3),
        0x49 => (ExclusiveOr(Immediate), 2),
        0x4a => (ShiftAccumulatorRight, 2),
        0x4c => (Jump(Absolute), 3),
        0x4d => (ExclusiveOr(Absolute), 4),
        0x4e => (ShiftRight(Absolute), 6),
        0x51 => (ExclusiveOr(IndirectIndexed), 5),
        0x55 => (ExclusiveOr(IndexedZeroPageX), 4),
        0x56 => (ShiftRight(IndexedZeroPageX), 6),
        0x59 => (ExclusiveOr(IndexedAbsoluteY), 4),
        0x5d => (ExclusiveOr(IndexedAbsoluteX), 4),
        0x5e => (ShiftRight(IndexedAbsoluteX), 7),
        0x60 => (ReturnFromSubroutine, 6),
        0x61 => (AddToAccumulatorWithCarry(IndexedIndirect), 6),
        0x65 => (AddToAccumulatorWithCarry(ZeroPage), 3),
        0x66 => (RotateRight(ZeroPage), 5),
        0x68 => (PopAccumulator, 4),
        0x69 => (AddToAccumulatorWithCarry(Immediate), 2),
        0x6a => (RotateAccumulatorRight, 2),
        0x6d => (AddToAccumulatorWithCarry(Absolute), 4),
        0x6e => (RotateRight(Absolute), 6),
        0x71 => (AddToAccumulatorWithCarry(IndirectIndexed), 5),
        0x75 => (AddToAccumulatorWithCarry(IndexedZeroPageX), 4),
        0x76 => (RotateRight(IndexedZeroPageX), 6),
        0x78 => (SetInterruptDisable, 2),
        0x79 => (AddToAccumulatorWithCarry(IndexedAbsoluteY), 4),
        0x7d => (AddToAccumulatorWithCarry(IndexedAbsoluteX), 4),
        0x7e => (RotateRight(IndexedAbsoluteX), 7),
        0x81 => (StoreAccumulator(IndexedIndirect), 6),
        0x84 => (StoreY(ZeroPage), 3),
        0x85 => (StoreAccumulator(ZeroPage), 3),
        0x86 => (StoreX(ZeroPage), 3),
        0x88 => (DecrementY, 2),
        0x8a => (RegisterTransfer(X, Accumulator), 2),
        0x8c => (StoreY(Absolute), 4),
        0x8d => (StoreAccumulator(Absolute), 4),
        0x8e => (StoreX(Absolute), 4),
        0x90 => (Branch(CarryClear), 2),
        0x91 => (StoreAccumulator(IndirectIndexed), 6),
        0x94 => (StoreY(IndexedZeroPageX), 4),
        0x95 => (StoreAccumulator(IndexedZeroPageX), 4),
        0x96 => (StoreX(IndexedZeroPageY), 4),
        0x98 => (RegisterTransfer(Y, Accumulator), 2),
        0x99 => (StoreAccumulator(IndexedAbsoluteY), 5),
        0x9a => (RegisterTransfer(X, Stack), 2),
        0x9d => (StoreAccumulator(IndexedAbsoluteX), 5),
        0xa0 => (LoadY(Immediate), 2),
        0xa1 => (LoadAccumulator(IndexedIndirect), 6),
        0xa2 => (LoadX(Immediate), 2),
        0xa5 => (LoadAccumulator(ZeroPage), 3),
        0xa6 => (LoadX(ZeroPage), 3),
        0xa8 => (RegisterTransfer(Accumulator, Y), 2),
        0xaa => (RegisterTransfer(Accumulator, X), 2),
        0xad => (LoadAccumulator(Absolute), 4),
        0xae => (LoadX(Absolute), 4),
        0xa9 => (LoadAccumulator(Immediate), 2),
        0xb0 => (Branch(CarrySet), 2),
        0xb1 => (LoadAccumulator(IndirectIndexed), 5),
        0xb5 => (LoadAccumulator(IndexedZeroPageX), 4),
        0xb6 => (LoadX(IndexedZeroPageY), 4),
        0xb9 => (LoadAccumulator(IndexedAbsoluteY), 4),
        0xbd => (LoadAccumulator(IndexedAbsoluteX), 4),
        0xbe => (LoadX(IndexedAbsoluteY), 4),
        0xc0 => (CompareWithY(Immediate), 2),
        0xc1 => (CompareWithAccumulator(IndexedIndirect), 6),
        0xc4 => (CompareWithY(ZeroPage), 3),
        0xc5 => (CompareWithAccumulator(ZeroPage), 3),
        0xc6 => (DecrementMemory(ZeroPage), 5),
        0xc8 => (IncrementY, 2),
        0xc9 => (CompareWithAccumulator(Immediate), 2),
        0xca => (DecrementX, 2),
        0xcc => (CompareWithY(Absolute), 4),
        0xcd => (CompareWithAccumulator(Absolute), 4),
        0xd0 => (Branch(ResultNotZero), 2),
        0xd1 => (CompareWithAccumulator(IndirectIndexed), 5),
        0xd5 => (CompareWithAccumulator(IndexedZeroPageX), 4),
        0xd8 => (ClearDecimal, 2),
        0xd9 => (CompareWithAccumulator(IndexedAbsoluteY), 4),
        0xdd => (CompareWithAccumulator(IndexedAbsoluteX), 4),
        0xe0 => (CompareWithX(Immediate), 2),
        0xe4 => (CompareWithX(ZeroPage), 3),
        0xe6 => (IncrementMemory(ZeroPage), 5),
        0xe8 => (IncrementX, 2),
        0xea => (NoOp, 2),
        0xec => (CompareWithX(Absolute), 4),
        0xee => (IncrementMemory(Absolute), 6),
        0xf0 => (Branch(ResultZero), 2),
        0xf6 => (IncrementMemory(IndexedZeroPageX), 6),
        0xfe => (IncrementMemory(IndexedAbsoluteX), 7),
        _ => unimplemented!("unrecognized opcode {:#04X}", opcode),
    };

    InstructionInfo {
        inst,
        cycles_remaining: cycles,
        address: cpu.pc,
    }
}
