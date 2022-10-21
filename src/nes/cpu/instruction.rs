use super::{Cpu, Register};

use log;

pub(super) struct InstructionInfo {
    pub inst: Instruction,
    pub cycles_remaining: usize,
    pub address: u16,
    disassembly: Option<Disassembly>,
}

impl Default for InstructionInfo {
    fn default() -> Self {
        InstructionInfo {
            inst: Instruction::Initial,
            cycles_remaining: 0,
            address: 0,
            disassembly: None,
        }
    }
}

impl std::fmt::Debug for InstructionInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InstructionInfo: {{ inst: {:?}, cycles_remaining: {}, address: {:#06X}, disassembly: {:?} }}",
               self.inst,
               self.cycles_remaining,
               self.address,
               self.disassembly)
    }
}

impl InstructionInfo {
    pub fn disasm(&self) {
        if let Some(d) = &self.disassembly {
            log::trace!("pc={:#06X} {:?}", self.address, d);
        } else {
            log::trace!("disassembly disabled");
        }
    }
}

pub(super) struct Disassembly {
    target_value: Option<u8>,
    target_addr: Option<u16>,
    asm: &'static str,
}

impl Disassembly {
    fn new(i: &Instruction, cpu: &Cpu) -> Self {
        use Instruction::*;
        use Register::*;

        let (target_value, target_addr, asm) = match *i {
            Initial => unreachable!(),
            SetInterruptDisable => (None, None, "SEI"),
            SetCarry => (None, None, "SEC"),
            SetDecimal => (None, None, "SED"),
            ClearInterruptDisable => (None, None, "CLI"),
            ClearDecimal => (None, None, "CLD"),
            ClearCarry => (None, None, "CLC"),
            ClearOverflow => (None, None, "CLV"),
            LoadAccumulator(am) => Self::get_disam_tuple(&am, cpu, "LDA"),
            StoreAccumulator(am) => (Some(cpu.a), Some(am.get_target_addr(cpu)), "STA"),
            PushAccumulator => (None, None, "PHA"),
            PopAccumulator => (None, None, "PLA"),
            PushStatus => (None, None, "PHP"),
            PopStatus => (None, None, "PLP"),
            LoadX(am) => Self::get_disam_tuple(&am, cpu, "LDX"),
            StoreX(am) => (Some(cpu.x), Some(am.get_target_addr(cpu)), "STX"),
            RegisterTransfer(src, dst) => {
                let s = match src {
                    Accumulator => match dst {
                        X => "TAX",
                        Y => "TAY",
                        _ => unreachable!(),
                    },
                    Stack => {
                        if matches!(dst, X) {
                            "TSX"
                        } else {
                            unreachable!()
                        }
                    }
                    X => match dst {
                        Accumulator => "TXA",
                        Stack => "TXS",
                        _ => unreachable!(),
                    },
                    Y => {
                        if matches!(dst, Accumulator) {
                            "TYA"
                        } else {
                            unreachable!()
                        }
                    }
                };

                (Some(cpu.get_register(src)), None, s)
            }
            LoadY(am) => Self::get_disam_tuple(&am, cpu, "LDY"),
            StoreY(am) => (Some(cpu.y), Some(am.get_target_addr(cpu)), "STY"),
            DecrementX => (None, None, "DEX"),
            DecrementY => (None, None, "DEY"),
            DecrementMemory(am) => (None, Some(am.get_target_addr(cpu)), "DEC"),
            IncrementX => (None, None, "INX"),
            IncrementY => (None, None, "INY"),
            IncrementMemory(am) => (None, Some(am.get_target_addr(cpu)), "INC"),
            And(am) => Self::get_disam_tuple(&am, cpu, "AND"),
            Branch(btype) => unreachable!(),
            JumpSavingReturn => (None, Some(cpu.memory.read_u16(cpu.pc + 1)), "JSR"),
            ReturnFromSubroutine => (None, None, "RTS"),
            Jump(am) => (None, Some(am.get_target_addr(cpu)), "JMP"),
            CompareWithAccumulator(am) => Self::get_disam_tuple(&am, cpu, "CMP"),
            CompareWithX(am) => Self::get_disam_tuple(&am, cpu, "CPX"),
            CompareWithY(am) => Self::get_disam_tuple(&am, cpu, "CPY"),
            ShiftAccumulatorRight => (None, None, "LSR A"),
            ShiftAccumulatorLeft => (None, None, "ASL A"),
            ShiftRight(am) => (None, Some(am.get_target_addr(cpu)), "LSR"),
            ShiftLeft(am) => (None, Some(am.get_target_addr(cpu)), "ASL"),
            AddToAccumulatorWithCarry(am) => Self::get_disam_tuple(&am, cpu, "ADC"),
            SubtractFromAccumulatorWithBorrow(am) => Self::get_disam_tuple(&am, cpu, "SBC"),
            Or(am) => Self::get_disam_tuple(&am, cpu, "ORA"),
            ExclusiveOr(am) => Self::get_disam_tuple(&am, cpu, "EOR"),
            RotateAccumulatorRight => (None, None, "ROR A"),
            RotateRight(am) => (None, Some(am.get_target_addr(cpu)), "ROR"),
            RotateAccumulatorLeft => (None, None, "ROL A"),
            RotateLeft(am) => (None, Some(am.get_target_addr(cpu)), "ROL"),
            TestBits(am) => (None, Some(am.get_target_addr(cpu)), "BIT"),
            ForceBreak => (None, None, "BRK"),
            ReturnFromInterrupt => (None, None, "RTI"),
            NoOp => (None, None, "NOP"),
            _ => unimplemented!("attempted to disassemble unrecognized instruction: {:?}", i),
        };

        Disassembly {
            target_value,
            target_addr,
            asm,
        }
    }

    fn from_branch_type(btype: &BranchType, cpu: &Cpu) -> Self {
        use BranchType::*;
        // add 1 to PC to advance past opcode byte
        let val = cpu.memory.read_u8(cpu.pc + 1) as i8;

        let target_addr = if val < 0 {
            cpu.pc + 2 - (val.abs() as u16)
        } else {
            cpu.pc + 2 + (val as u16)
        };

        let opcode = match btype {
            ResultZero => "BEQ",
            ResultNotZero => "BNE",
            ResultPlus => "BPL",
            ResultMinus => "BMI",
            CarryClear => "BCC",
            CarrySet => "BCS",
            OverflowClear => "BVC",
            OverflowSet => "BVS",
        };

        Disassembly {
            target_value: None,
            target_addr: Some(target_addr),
            asm: opcode,
        }
    }

    fn get_disam_tuple(
        am: &AddressingMode,
        cpu: &Cpu,
        opcode: &'static str,
    ) -> (Option<u8>, Option<u16>, &'static str) {
        use AddressingMode::*;

        match *am {
            Immediate => (Some(am.get_value(cpu)), None, opcode),
            _ => (
                Some(am.get_value(cpu)),
                Some(am.get_target_addr(cpu)),
                opcode,
            ),
        }
    }
}

impl std::fmt::Debug for Disassembly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.asm)?;
        if let Some(a) = &self.target_addr {
            write!(f, " (a={:#06X})", a)?;
        };
        if let Some(v) = &self.target_value {
            write!(f, " [v={:#04X}]", v)?;
        };
        if self.target_addr.is_none() && self.target_value.is_some() {
            write!(f, " {{imm}}")?
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub(super) enum Instruction {
    Initial,
    SetInterruptDisable,
    SetCarry,
    SetDecimal,
    ClearInterruptDisable,
    ClearCarry,
    ClearDecimal,
    ClearOverflow,
    LoadAccumulator(AddressingMode),
    StoreAccumulator(AddressingMode),
    PushAccumulator,
    PopAccumulator,
    PushStatus,
    PopStatus,
    LoadX(AddressingMode),
    StoreX(AddressingMode),
    // src, dest
    RegisterTransfer(Register, Register),
    LoadY(AddressingMode),
    StoreY(AddressingMode),
    DecrementX,
    DecrementY,
    DecrementMemory(AddressingMode),
    IncrementX,
    IncrementY,
    IncrementMemory(AddressingMode),
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
    SubtractFromAccumulatorWithBorrow(AddressingMode),
    Or(AddressingMode),
    ExclusiveOr(AddressingMode),
    RotateAccumulatorRight,
    RotateRight(AddressingMode),
    RotateAccumulatorLeft,
    RotateLeft(AddressingMode),
    TestBits(AddressingMode),
    ForceBreak,
    ReturnFromInterrupt,
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
    OverflowClear,
    OverflowSet,
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
    fn get_target_addr(&self, cpu: &Cpu) -> u16 {
        use AddressingMode::*;

        // increment by one to lookahead past the opcode byte
        let pc = cpu.pc + 1;

        match *self {
            ZeroPage => {
                let target_addr = cpu.memory.read_u8(pc) as u16;
                target_addr
            }
            IndexedZeroPageX => {
                let target_addr = cpu.memory.read_u8(pc).wrapping_add(cpu.x) as u16;
                target_addr
            }
            IndexedZeroPageY => {
                let target_addr = cpu.memory.read_u8(pc).wrapping_add(cpu.y) as u16;
                target_addr
            }
            Absolute => {
                let target_addr = cpu.memory.read_u16(pc);
                target_addr
            }
            IndexedAbsoluteX => {
                let target_addr = cpu.memory.read_u16(pc).wrapping_add(cpu.x as u16);
                target_addr
            }
            IndexedAbsoluteY => {
                let target_addr = cpu.memory.read_u16(pc).wrapping_add(cpu.y as u16);
                target_addr
            }
            Indirect => {
                let indirect_addr = cpu.memory.read_u16(pc);
                cpu.memory.read_u16(indirect_addr)
            }
            IndexedIndirect => {
                let indirect_addr = cpu.memory.read_u8(pc).wrapping_add(cpu.x) as u16;
                cpu.memory.read_u16(indirect_addr)
            }
            IndirectIndexed => {
                let indirect_base = cpu.memory.read_u8(pc) as u16;
                cpu.memory
                    .read_u16(indirect_base)
                    .wrapping_add(cpu.y as u16)
            }
            Immediate => unreachable!(),
        }
    }

    /// Returns the target address for the operation based on mode. Mutates the passed CPU to update the
    /// program counter for consumed operands.
    pub(super) fn get_target_addr_and_adjust_pc(&self, cpu: &mut Cpu) -> u16 {
        use AddressingMode::*;

        let target_addr = self.get_target_addr(cpu);

        match *self {
            ZeroPage => {
                cpu.pc += 2;
            }
            IndexedZeroPageX => {
                cpu.pc += 2;
            }
            IndexedZeroPageY => {
                cpu.pc += 2;
            }
            Absolute => {
                cpu.pc += 3;
            }
            IndexedAbsoluteX => {
                cpu.pc += 3;
            }
            IndexedAbsoluteY => {
                cpu.pc += 3;
            }
            Indirect => {
                cpu.pc += 3;
            }
            IndexedIndirect => {
                cpu.pc += 2;
            }
            IndirectIndexed => {
                cpu.pc += 2;
            }
            Immediate => unreachable!(),
        }

        target_addr
    }

    fn get_value(&self, cpu: &Cpu) -> u8 {
        use AddressingMode::*;

        // increment by one to lookahead past the opcode byte
        let pc = cpu.pc + 1;

        match *self {
            ZeroPage => {
                let target_addr = cpu.memory.read_u8(pc) as u16;
                cpu.memory.read_u8(target_addr)
            }
            IndexedZeroPageX => {
                let target_addr = cpu.memory.read_u8(pc).wrapping_add(cpu.x) as u16;
                cpu.memory.read_u8(target_addr)
            }
            IndexedZeroPageY => {
                let target_addr = cpu.memory.read_u8(pc).wrapping_add(cpu.y) as u16;
                cpu.memory.read_u8(target_addr)
            }
            Absolute => {
                let target_addr = cpu.memory.read_u16(pc);
                cpu.memory.read_u8(target_addr)
            }
            IndexedAbsoluteX => {
                let target_addr = cpu.memory.read_u16(pc).wrapping_add(cpu.x as u16);
                cpu.memory.read_u8(target_addr)
            }
            IndexedAbsoluteY => {
                let target_addr = cpu.memory.read_u16(pc).wrapping_add(cpu.y as u16);
                cpu.memory.read_u8(target_addr)
            }
            Indirect => {
                let indirect_addr = cpu.memory.read_u16(pc);
                let target_addr = cpu.memory.read_u16(indirect_addr);
                cpu.memory.read_u8(target_addr)
            }
            IndexedIndirect => {
                let indirect_addr = cpu.memory.read_u8(pc).wrapping_add(cpu.x) as u16;
                let target_addr = cpu.memory.read_u16(indirect_addr);
                cpu.memory.read_u8(target_addr)
            }
            IndirectIndexed => {
                let indirect_base = cpu.memory.read_u8(pc) as u16;
                let target_addr = cpu
                    .memory
                    .read_u16(indirect_base)
                    .wrapping_add(cpu.y as u16);
                cpu.memory.read_u8(target_addr)
            }
            Immediate => cpu.memory.read_u8(pc),
        }
    }

    /// Returns the value for the operation based on mode. Mutates the passed CPU to update the
    /// program counter for consumed opcode and operands.
    pub(super) fn get_value_and_adjust_pc(&self, cpu: &mut Cpu) -> u8 {
        use AddressingMode::*;

        let value = self.get_value(cpu);

        match *self {
            ZeroPage => {
                cpu.pc += 2;
            }
            IndexedZeroPageX => {
                cpu.pc += 2;
            }
            IndexedZeroPageY => {
                cpu.pc += 2;
            }
            Absolute => {
                cpu.pc += 3;
            }
            IndexedAbsoluteX => {
                cpu.pc += 3;
            }
            IndexedAbsoluteY => {
                cpu.pc += 3;
            }
            Indirect => {
                cpu.pc += 3;
            }
            IndexedIndirect => {
                cpu.pc += 2;
            }
            IndirectIndexed => {
                cpu.pc += 2;
            }
            Immediate => {
                cpu.pc += 2;
            }
        }

        value
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
        0x00 => (ForceBreak, 7),
        0x01 => (Or(IndexedIndirect), 6),
        0x05 => (Or(ZeroPage), 3),
        0x06 => (ShiftLeft(ZeroPage), 5),
        0x08 => (PushStatus, 3),
        0x09 => (Or(Immediate), 2),
        0x0a => (ShiftAccumulatorLeft, 2),
        0x0d => (Or(Absolute), 4),
        0x0e => (ShiftLeft(Absolute), 6),
        0x10 => (Branch(ResultPlus), 2),
        0x11 => (Or(IndirectIndexed), 5),
        0x15 => (Or(IndexedZeroPageX), 4),
        0x16 => (ShiftLeft(IndexedZeroPageX), 6),
        0x18 => (ClearCarry, 2),
        0x19 => (Or(IndexedAbsoluteY), 4),
        0x1d => (Or(IndexedAbsoluteX), 4),
        0x1e => (ShiftLeft(IndexedAbsoluteX), 7),
        0x20 => (JumpSavingReturn, 6),
        0x21 => (And(IndexedIndirect), 6),
        0x24 => (TestBits(ZeroPage), 3),
        0x25 => (And(ZeroPage), 3),
        0x26 => (RotateLeft(ZeroPage), 5),
        0x28 => (PopStatus, 4),
        0x29 => (And(Immediate), 2),
        0x2a => (RotateAccumulatorLeft, 2),
        0x2c => (TestBits(Absolute), 4),
        0x2d => (And(Absolute), 4),
        0x2e => (RotateLeft(Absolute), 6),
        0x30 => (Branch(ResultMinus), 2),
        0x31 => (And(IndirectIndexed), 5),
        0x35 => (And(IndexedZeroPageX), 4),
        0x36 => (RotateLeft(IndexedZeroPageX), 6),
        0x38 => (SetCarry, 2),
        0x39 => (And(IndexedAbsoluteY), 4),
        0x3d => (And(IndexedAbsoluteX), 4),
        0x3e => (RotateLeft(IndexedAbsoluteX), 7),
        0x40 => (ReturnFromInterrupt, 6),
        0x41 => (ExclusiveOr(IndexedIndirect), 6),
        0x45 => (ExclusiveOr(ZeroPage), 3),
        0x46 => (ShiftRight(ZeroPage), 5),
        0x48 => (PushAccumulator, 3),
        0x49 => (ExclusiveOr(Immediate), 2),
        0x4a => (ShiftAccumulatorRight, 2),
        0x4c => (Jump(Absolute), 3),
        0x4d => (ExclusiveOr(Absolute), 4),
        0x4e => (ShiftRight(Absolute), 6),
        0x50 => (Branch(OverflowClear), 2),
        0x51 => (ExclusiveOr(IndirectIndexed), 5),
        0x55 => (ExclusiveOr(IndexedZeroPageX), 4),
        0x56 => (ShiftRight(IndexedZeroPageX), 6),
        0x58 => (ClearInterruptDisable, 2),
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
        0x6c => (Jump(Indirect), 5),
        0x6d => (AddToAccumulatorWithCarry(Absolute), 4),
        0x6e => (RotateRight(Absolute), 6),
        0x70 => (Branch(OverflowSet), 2),
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
        0xa4 => (LoadY(ZeroPage), 3),
        0xa5 => (LoadAccumulator(ZeroPage), 3),
        0xa6 => (LoadX(ZeroPage), 3),
        0xa8 => (RegisterTransfer(Accumulator, Y), 2),
        0xaa => (RegisterTransfer(Accumulator, X), 2),
        0xac => (LoadY(Absolute), 4),
        0xad => (LoadAccumulator(Absolute), 4),
        0xae => (LoadX(Absolute), 4),
        0xa9 => (LoadAccumulator(Immediate), 2),
        0xb0 => (Branch(CarrySet), 2),
        0xb1 => (LoadAccumulator(IndirectIndexed), 5),
        0xb4 => (LoadX(IndexedZeroPageX), 4),
        0xb5 => (LoadAccumulator(IndexedZeroPageX), 4),
        0xb6 => (LoadX(IndexedZeroPageY), 4),
        0xb8 => (ClearOverflow, 2),
        0xb9 => (LoadAccumulator(IndexedAbsoluteY), 4),
        0xba => (RegisterTransfer(Stack, X), 2),
        0xbc => (LoadY(IndexedAbsoluteX), 4),
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
        0xce => (DecrementMemory(Absolute), 6),
        0xd0 => (Branch(ResultNotZero), 2),
        0xd1 => (CompareWithAccumulator(IndirectIndexed), 5),
        0xd5 => (CompareWithAccumulator(IndexedZeroPageX), 4),
        0xd6 => (DecrementMemory(IndexedZeroPageX), 6),
        0xd8 => (ClearDecimal, 2),
        0xd9 => (CompareWithAccumulator(IndexedAbsoluteY), 4),
        0xdd => (CompareWithAccumulator(IndexedAbsoluteX), 4),
        0xde => (DecrementMemory(IndexedAbsoluteX), 7),
        0xe0 => (CompareWithX(Immediate), 2),
        0xe1 => (SubtractFromAccumulatorWithBorrow(IndexedIndirect), 6),
        0xe4 => (CompareWithX(ZeroPage), 3),
        0xe5 => (SubtractFromAccumulatorWithBorrow(ZeroPage), 3),
        0xe6 => (IncrementMemory(ZeroPage), 5),
        0xe8 => (IncrementX, 2),
        0xe9 => (SubtractFromAccumulatorWithBorrow(Immediate), 2),
        0xea => (NoOp, 2),
        0xec => (CompareWithX(Absolute), 4),
        0xed => (SubtractFromAccumulatorWithBorrow(Absolute), 4),
        0xee => (IncrementMemory(Absolute), 6),
        0xf0 => (Branch(ResultZero), 2),
        0xf1 => (SubtractFromAccumulatorWithBorrow(IndirectIndexed), 5),
        0xf5 => (SubtractFromAccumulatorWithBorrow(IndexedZeroPageX), 4),
        0xf6 => (IncrementMemory(IndexedZeroPageX), 6),
        0xf8 => (SetDecimal, 2),
        0xf9 => (SubtractFromAccumulatorWithBorrow(IndexedAbsoluteY), 4),
        0xfd => (SubtractFromAccumulatorWithBorrow(IndexedAbsoluteX), 4),
        0xfe => (IncrementMemory(IndexedAbsoluteX), 7),
        _ => unimplemented!("unrecognized opcode {:#04X}", opcode),
    };

    let disassembly = if !cpu.disassemble || matches!(inst, Initial) {
        None
    } else if let Branch(btype) = inst {
        Some(Disassembly::from_branch_type(&btype, cpu))
    } else {
        Some(Disassembly::new(&inst, cpu))
    };

    InstructionInfo {
        inst,
        cycles_remaining: cycles,
        address: cpu.pc,
        disassembly,
    }
}
