use log::trace;

use super::flags;
use super::Cpu;

#[derive(Debug, Copy, Clone)]
pub(super) enum Instruction {
    SetInterruptDisable,
    ClearDecimal,
    LoadAccumulator(AddressingMode),
    StoreAccumulator(AddressingMode),
    LoadX(AddressingMode),
    StoreX(AddressingMode),
    TransferXToStackPointer,
    LoadY(AddressingMode),
    StoreY(AddressingMode),
    DecrementY,
    DecrementMemory(AddressingMode),
    And(AddressingMode),
    Branch(BranchType),
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
    Accumulator,
    Immediate,
    IndexedIndirect,
    IndirectIndexed,
}

#[derive(Debug, Copy, Clone)]
pub(super) enum BranchType {
    ResultZero,
    ResultNotZero,
    ResultPositive,
}

pub(super) fn get_instruction(opcode: u8) -> Instruction {
    use AddressingMode::*;
    use BranchType::*;
    use Instruction::*;

    match opcode {
        0x10 => Branch(ResultPositive),
        0x29 => And(Immediate),
        0x78 => SetInterruptDisable,
        0x84 => StoreY(ZeroPage),
        0x88 => DecrementY,
        0x8d => StoreAccumulator(Absolute),
        0x91 => StoreAccumulator(IndirectIndexed),
        0x9a => TransferXToStackPointer,
        0xa0 => LoadY(Immediate),
        0xa2 => LoadX(Immediate),
        0xad => LoadAccumulator(Absolute),
        0xa9 => LoadAccumulator(Immediate),
        0xc6 => DecrementMemory(ZeroPage),
        0xd0 => Branch(ResultNotZero),
        0xd8 => ClearDecimal,
        0xf0 => Branch(ResultZero),
        _ => unimplemented!("unrecognized opcode {:#04X}", opcode),
    }
}

fn update_nz_flags(cpu: &mut Cpu, val: u8) {
    if val == 0 {
        cpu.flags |= flags::ZERO;
    } else {
        cpu.flags &= !flags::ZERO;
    }

    if val & 0b10000000 > 0 {
        cpu.flags |= flags::NEGATIVE;
    } else {
        cpu.flags &= !flags::NEGATIVE;
    }
}

impl Instruction {
    pub(super) fn execute(&self, cpu: &mut Cpu) -> usize {
        use AddressingMode::*;
        use BranchType::*;
        use Instruction::*;

        cpu.pc += 1;
        match *self {
            SetInterruptDisable => {
                trace!("SEI");
                cpu.flags |= flags::INTERRUPT_DISABLE;
                2
            }
            ClearDecimal => {
                trace!("CLD");
                cpu.flags &= !flags::DECIMAL;
                2
            }
            LoadAccumulator(am) => {
                let (val, cycles) = match am {
                    Immediate => {
                        let v = cpu.memory.read_u8(cpu.pc);
                        cpu.pc += 1;
                        (v, 2)
                    }
                    Absolute => {
                        let addr = cpu.memory.read_u16(cpu.pc);
                        cpu.pc += 2;
                        let v = cpu.memory.read_u8(addr);
                        (v, 4)
                    }
                    ZeroPage | IndexedZeroPageX | IndexedAbsoluteX | IndexedAbsoluteY
                    | IndexedIndirect | IndirectIndexed => unimplemented!(),
                    _ => unreachable!(),
                };

                trace!("LDA {:#04X}", val);
                cpu.a = val;
                update_nz_flags(cpu, val);
                cycles
            }
            StoreAccumulator(am) => {
                let (addr, cycles) = match am {
                    Absolute => {
                        let addr = cpu.memory.read_u16(cpu.pc);
                        cpu.pc += 2;
                        (addr, 4)
                    }
                    IndirectIndexed => {
                        let z_page_index = cpu.memory.read_u8(cpu.pc) as u16;
                        cpu.pc += 1;
                        let addr = cpu.memory.read_u16(z_page_index).wrapping_add(cpu.y as u16);
                        (addr, 6)
                    }
                    ZeroPage | IndexedZeroPageX | IndexedAbsoluteX | IndexedAbsoluteY
                    | IndexedIndirect => unimplemented!(),
                    _ => unreachable!(),
                };

                trace!("STA {:#06X}", addr);
                cpu.memory.write_u8(addr, cpu.a);
                cycles
            }
            LoadX(am) => {
                let (val, cycles) = match am {
                    Immediate => {
                        let v = cpu.memory.read_u8(cpu.pc);
                        cpu.pc += 1;
                        (v, 2)
                    }
                    ZeroPage | IndexedZeroPageY | Absolute | IndexedAbsoluteY => unimplemented!(),
                    _ => unreachable!(),
                };

                trace!("LDX {:#04X}", val);
                cpu.x = val;
                update_nz_flags(cpu, val);
                cycles
            }
            StoreX(am) => unimplemented!(),
            TransferXToStackPointer => {
                trace!("TXS");
                cpu.sp = cpu.x;
                2
            }
            LoadY(am) => {
                let (val, cycles) = match am {
                    Immediate => {
                        let v = cpu.memory.read_u8(cpu.pc);
                        cpu.pc += 1;
                        (v, 2)
                    }
                    ZeroPage | IndexedZeroPageX | Absolute | IndexedAbsoluteX => unimplemented!(),
                    _ => unreachable!(),
                };

                trace!("LDY {:#04X}", val);
                cpu.y = val;
                update_nz_flags(cpu, val);
                cycles
            }
            StoreY(am) => {
                let (addr, cycles) = match am {
                    ZeroPage => {
                        let addr = cpu.memory.read_u8(cpu.pc) as u16;
                        cpu.pc += 1;
                        (addr, 3)
                    }
                    IndexedZeroPageX | Absolute => unimplemented!(),
                    _ => unreachable!(),
                };

                trace!("STY {:#06X}", addr);
                cpu.memory.write_u8(addr, cpu.y);
                cycles
            }
            DecrementY => {
                trace!("DEY");
                cpu.y = cpu.y.wrapping_sub(1);
                update_nz_flags(cpu, cpu.y);
                2
            }
            DecrementMemory(am) => {
                let (addr, cycles) = match am {
                    ZeroPage => {
                        let addr = cpu.memory.read_u8(cpu.pc) as u16;
                        cpu.pc += 1;
                        (addr, 5)
                    }
                    IndexedZeroPageX | Absolute | IndexedAbsoluteX => unimplemented!(),
                    _ => unreachable!(),
                };

                trace!("DEC {:#06X}", addr);
                let new_val = cpu.memory.read_u8(addr).wrapping_sub(1);
                cpu.memory.write_u8(addr, new_val);
                update_nz_flags(cpu, new_val);
                cycles
            }
            And(am) => {
                let (val, cycles) = match am {
                    Immediate => {
                        let v = cpu.memory.read_u8(cpu.pc);
                        cpu.pc += 1;
                        (v, 2)
                    }
                    ZeroPage | IndexedZeroPageX | Absolute | IndexedAbsoluteX
                    | IndexedAbsoluteY | IndirectIndexed | IndexedIndirect => unimplemented!(),
                    _ => unreachable!(),
                };

                trace!("AND {:#04X}", val);
                let r = cpu.a & val;
                cpu.a = r;
                update_nz_flags(cpu, r);
                cycles
            }
            Branch(b) => {
                let val = cpu.memory.read_u8(cpu.pc) as i8;
                cpu.pc += 1;

                let new_pc = if val < 0 {
                    cpu.pc - (val.abs() as u16)
                } else {
                    cpu.pc + (val as u16)
                };

                //TODO: handle cycle differences depending on which page the jump targets
                let cycles = match b {
                    ResultZero => {
                        log::trace!("BEQ {:#06X}", new_pc);
                        if cpu.flags & flags::ZERO > 0 {
                            cpu.pc = new_pc;
                        }
                        2
                    }
                    ResultNotZero => {
                        log::trace!("BNE {:#06X}", new_pc);
                        if cpu.flags & flags::ZERO == 0 {
                            cpu.pc = new_pc;
                        }
                        2
                    }
                    ResultPositive => {
                        log::trace!("BPL {:#06X}", new_pc);
                        if cpu.flags & flags::NEGATIVE == 0 {
                            cpu.pc = new_pc;
                        }
                        2
                    }
                };

                cycles
            }
        }
    }
}
