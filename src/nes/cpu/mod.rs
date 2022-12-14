mod instruction;
mod memory;

use log;
use log::trace;

use crate::nes::cartridge::Cartridge;
use memory::Memory;

const IRQ_VECTOR: u16 = 0xFFFA;
const RST_VECTOR: u16 = 0xFFFC;
const NMI_VECTOR: u16 = 0xFFFE;

#[derive(Copy, Clone, PartialEq, Eq)]
pub(super) enum Register {
    Stack,
    Accumulator,
    X,
    Y,
}

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Register {
    pub const fn as_str(&self) -> &'static str {
        use Register::*;

        match self {
            Stack => "S",
            Accumulator => "A",
            X => "X",
            Y => "Y",
        }
    }
}

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    pc: u16,
    flags: u8,
    memory: Memory,
    breakpoint: u16,
    disassemble: bool,
    current_instruction: instruction::InstructionInfo,
}

impl std::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "CPU \n  a: {:#04X} x: {:#04X} y: {:#04X}",
            self.a, self.x, self.y
        )?;
        writeln!(f, "  sp: {:#04X} pc: {:#06X}", self.sp, self.pc)?;
        write!(
            f,
            "  C: {} Z: {} I: {} D: {} V: {} N: {}",
            self.flags & flags::CARRY > 0,
            self.flags & flags::ZERO > 0,
            self.flags & flags::INTERRUPT_DISABLE > 0,
            self.flags & flags::DECIMAL > 0,
            self.flags & flags::OVERFLOW > 0,
            self.flags & flags::NEGATIVE > 0
        )
    }
}

mod flags {
    pub const CARRY: u8 = 0b00000001;
    pub const ZERO: u8 = 0b00000010;
    pub const INTERRUPT_DISABLE: u8 = 0b00000100;
    pub const DECIMAL: u8 = 0b00001000;
    pub const B: u8 = 0b00010000;
    pub const OVERFLOW: u8 = 0b01000000;
    pub const NEGATIVE: u8 = 0b10000000;
}

impl Cpu {
    pub fn new(cartridge: Cartridge, disassemble: bool) -> Self {
        let mut cpu = Cpu {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xfd,
            pc: 0,
            flags: 0,
            memory: Memory::new(cartridge),
            breakpoint: 0,
            disassemble,
            current_instruction: instruction::InstructionInfo::default(),
        };
        cpu.reset();
        cpu
    }

    pub fn set_breakpoint(&mut self, b: u16) {
        self.breakpoint = b;
    }

    fn set_b_flag(&mut self, bit_5: bool, bit_4: bool) {
        let mut mask = 0b11001111;
        if bit_4 {
            mask = mask | 0b00010000;
        }
        if bit_5 {
            mask = mask | 0b00100000;
        }
        self.flags &= mask;
    }

    fn reset(&mut self) {
        self.sp = 0xfd;
        self.pc = self.memory.read_u16(RST_VECTOR);
        self.flags |= flags::INTERRUPT_DISABLE;
        self.set_b_flag(true, false);
    }

    fn push(&mut self, val: u8) {
        self.sp = self.sp.wrapping_sub(1);
        self.memory.write_u8(self.sp as u16 + 0x100, val);
    }

    fn pop(&mut self) -> u8 {
        let v = self.memory.read_u8(self.sp as u16 + 0x100);
        self.sp = self.sp.wrapping_add(1);
        v
    }

    fn get_register(&self, r: Register) -> u8 {
        use Register::*;

        match r {
            X => self.x,
            Y => self.y,
            Stack => self.sp,
            Accumulator => self.a,
        }
    }

    fn set_register(&mut self, r: Register, v: u8) {
        use Register::*;

        match r {
            X => self.x = v,
            Y => self.y = v,
            Stack => self.sp = v,
            Accumulator => self.a = v,
        };
    }

    pub fn tick(&mut self, trace_cpu: bool) {
        let breakpoint = self.pc == self.breakpoint;

        if self.current_instruction.cycles_remaining == 0 {
            let i = instruction::get_instruction(self);
            self.current_instruction = i;
        }

        self.current_instruction.cycles_remaining -= 1;
        if self.current_instruction.cycles_remaining == 0 {
            self.execute_instruction();

            if breakpoint {
                println!(
                    "breaking after instruction at specified breakpoint: {:#06X}",
                    self.breakpoint
                );
                println!("instruction: {:?}", self.current_instruction);
                println!("{:?}", self);
                println!("stack: {:#X?}", self.memory.get_stack_slice(self.sp));
                panic!();
            }
        }

        if trace_cpu {
            trace!("{:#?}", self);
        }
    }

    fn update_nz_flags(&mut self, val: u8) {
        if val == 0 {
            self.flags |= flags::ZERO;
        } else {
            self.flags &= !flags::ZERO;
        }

        if val & 0b10000000 > 0 {
            self.flags |= flags::NEGATIVE;
        } else {
            self.flags &= !flags::NEGATIVE;
        }
    }

    fn update_shift_flags(&mut self, carry: bool, val: u8) {
        self.flags &= !flags::NEGATIVE;

        if carry {
            self.flags |= flags::CARRY;
        } else {
            self.flags &= !flags::CARRY;
        }

        if val == 0 {
            self.flags |= flags::ZERO;
        } else {
            self.flags &= !flags::ZERO;
        }
    }

    fn compare(&mut self, reg_val: u8, mem_val: u8) {
        if reg_val >= mem_val {
            self.flags |= flags::CARRY;
        } else {
            self.flags &= !flags::CARRY;
        }
        self.update_nz_flags(reg_val.wrapping_sub(mem_val));
    }

    fn execute_instruction(&mut self) {
        use instruction::BranchType::*;
        use instruction::Instruction::*;

        if self.disassemble {
            self.current_instruction.disasm();
        }

        match self.current_instruction.inst {
            SetInterruptDisable => {
                self.flags |= flags::INTERRUPT_DISABLE;
                self.pc += 1;
            }
            SetCarry => {
                self.flags |= flags::CARRY;
                self.pc += 1;
            }
            SetDecimal => {
                self.flags |= flags::DECIMAL;
                self.pc += 1;
            }
            ClearInterruptDisable => {
                self.flags &= !flags::INTERRUPT_DISABLE;
                self.pc += 1;
            }
            ClearDecimal => {
                self.flags &= !flags::DECIMAL;
                self.pc += 1;
            }
            ClearCarry => {
                self.flags &= !flags::CARRY;
                self.pc += 1;
            }
            ClearOverflow => {
                self.flags &= !flags::OVERFLOW;
                self.pc += 1;
            }
            LoadAccumulator(am) => {
                let val = am.get_value_and_adjust_pc(self);
                self.a = val;
                self.update_nz_flags(val);
            }
            StoreAccumulator(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                self.memory.write_u8(addr, self.a);
            }
            PushAccumulator => {
                self.push(self.a);
                self.pc += 1;
            }
            PopAccumulator => {
                let v = self.pop();
                self.update_nz_flags(v);
                self.a = v;
                self.pc += 1;
            }
            PushStatus => {
                self.push(self.flags | flags::B | 0b00100000);
                self.pc += 1;
            }
            PopStatus => {
                self.flags = self.pop() & 0b11001111;

                self.pc += 1;
            }
            LoadX(am) => {
                let val = am.get_value_and_adjust_pc(self);
                self.x = val;
                self.update_nz_flags(val);
            }
            StoreX(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                self.memory.write_u8(addr, self.x);
            }
            RegisterTransfer(src, dst) => {
                let val = self.get_register(src);
                self.set_register(dst, val);

                if dst != Register::Stack {
                    self.update_nz_flags(val);
                }
                self.pc += 1;
            }
            LoadY(am) => {
                let val = am.get_value_and_adjust_pc(self);
                self.y = val;
                self.update_nz_flags(val);
            }
            StoreY(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                self.memory.write_u8(addr, self.y);
            }
            DecrementX => {
                self.x = self.x.wrapping_sub(1);
                self.update_nz_flags(self.x);
                self.pc += 1;
            }
            DecrementY => {
                self.y = self.y.wrapping_sub(1);
                self.update_nz_flags(self.y);
                self.pc += 1;
            }
            IncrementX => {
                self.x = self.x.wrapping_add(1);
                self.update_nz_flags(self.x);
                self.pc += 1;
            }
            IncrementY => {
                self.y = self.y.wrapping_add(1);
                self.update_nz_flags(self.y);
                self.pc += 1;
            }
            IncrementMemory(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                let new_val = self.memory.read_u8(addr).wrapping_add(1);
                self.memory.write_u8(addr, new_val);
                self.update_nz_flags(new_val);
            }
            DecrementMemory(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                let new_val = self.memory.read_u8(addr).wrapping_sub(1);
                self.memory.write_u8(addr, new_val);
                self.update_nz_flags(new_val);
            }
            And(am) => {
                let val = am.get_value_and_adjust_pc(self);
                let r = self.a & val;
                self.a = r;
                self.update_nz_flags(r);
            }
            Branch(b) => {
                // advance past opcode byte
                self.pc += 1;
                let val = self.memory.read_u8(self.pc) as i8;
                self.pc += 1;

                let new_pc = if val < 0 {
                    self.pc - (val.abs() as u16)
                } else {
                    self.pc + (val as u16)
                };

                match b {
                    ResultZero => {
                        if self.flags & flags::ZERO > 0 {
                            self.pc = new_pc;
                        }
                    }
                    ResultNotZero => {
                        if self.flags & flags::ZERO == 0 {
                            self.pc = new_pc;
                        }
                    }
                    ResultPlus => {
                        if self.flags & flags::NEGATIVE == 0 {
                            self.pc = new_pc;
                        }
                    }
                    ResultMinus => {
                        if self.flags & flags::NEGATIVE > 0 {
                            self.pc = new_pc;
                        }
                    }
                    CarryClear => {
                        if self.flags & flags::CARRY == 0 {
                            self.pc = new_pc;
                        }
                    }
                    CarrySet => {
                        if self.flags & flags::CARRY > 0 {
                            self.pc = new_pc;
                        }
                    }
                    OverflowClear => {
                        if self.flags & flags::OVERFLOW == 0 {
                            self.pc = new_pc;
                        }
                    }
                    OverflowSet => {
                        if self.flags & flags::OVERFLOW > 0 {
                            self.pc = new_pc;
                        }
                    }
                }
            }
            JumpSavingReturn => {
                // advance past opcode byte
                self.pc += 1;

                // get PC for jump and adjust for bytes read
                let new_pc = self.memory.read_u16(self.pc);
                self.pc += 2;

                // store return PC on stack
                let return_pc = self.current_instruction.address + 2;
                self.push((return_pc >> 8) as u8);
                self.push(return_pc as u8);

                // set new PC
                self.pc = new_pc;
            }
            ReturnFromSubroutine => {
                let lo = self.pop();
                let hi = self.pop();

                let new_pc = ((hi as u16) << 8 | lo as u16) + 1;
                self.pc = new_pc;
            }
            Jump(am) => {
                let new_pc = am.get_target_addr_and_adjust_pc(self);
                self.pc = new_pc;
            }
            CompareWithAccumulator(am) => {
                let val = am.get_value_and_adjust_pc(self);
                self.compare(self.a, val);
            }
            CompareWithX(am) => {
                let val = am.get_value_and_adjust_pc(self);
                self.compare(self.x, val);
            }
            CompareWithY(am) => {
                let val = am.get_value_and_adjust_pc(self);
                self.compare(self.y, val);
            }
            ShiftAccumulatorRight => {
                let carry = self.a & 1 > 0;
                let new_val = self.a >> 1;

                self.update_shift_flags(carry, new_val);
                self.a = new_val;
                self.pc += 1;
            }
            ShiftAccumulatorLeft => {
                let carry = self.a & 0x80 > 0;
                let new_val = self.a << 1;

                self.update_shift_flags(carry, new_val);
                self.a = new_val;
                self.pc += 1;
            }
            ShiftRight(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                let v = self.memory.read_u8(addr);
                let carry = v & 1 > 0;
                let new_val = v >> 1;

                self.update_shift_flags(carry, new_val);
                self.memory.write_u8(addr, new_val);
            }
            ShiftLeft(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                let v = self.memory.read_u8(addr);
                let carry = v & 0x80 > 0;
                let new_val = v << 1;

                self.update_shift_flags(carry, new_val);
                self.memory.write_u8(addr, new_val);
            }
            AddToAccumulatorWithCarry(am) => {
                let val = am.get_value_and_adjust_pc(self);
                let c = self.flags & 1;
                let acc = self.a;

                let new_val = acc.wrapping_add(val).wrapping_add(c);
                self.a = new_val;

                self.update_nz_flags(self.a);
                if acc as u16 + val as u16 + c as u16 > 0xFF {
                    self.flags |= flags::CARRY;
                } else {
                    self.flags &= !flags::CARRY;
                }

                // if the original accumulator and the added value share the same sign bit
                //          AND
                // the original accumulator and the new accumulator DO NOT share the same sign bit
                //      then the operation moved the value outside of the signed range and we have to set V
                //
                // if the original accumulator and the added value do not share the same sign bit, then they
                // cannot overflow the signed bounds
                if (acc ^ val) & 0x80 == 0 && (acc ^ self.a) & 0x80 != 0 {
                    self.flags |= flags::OVERFLOW;
                } else {
                    self.flags &= !flags::OVERFLOW;
                }
            }
            SubtractFromAccumulatorWithBorrow(am) => {
                let val = am.get_value_and_adjust_pc(self);
                let c = self.flags & 1;
                let acc = self.a;

                let new_val = acc.wrapping_sub(val).wrapping_sub(1 - c);
                self.a = new_val;

                self.update_nz_flags(self.a);
                if acc as i8 - val as i8 - (1 - c) as i8 >= 0 {
                    self.flags |= flags::CARRY;
                } else {
                    self.flags &= !flags::CARRY;
                }

                // if the original accumulator and the subtracted value DO NOT share the same sign bit
                //          AND
                // the original accumulator and the new accumulator DO NOT share the same sign bit
                //      then the operation moved the value outside of the signed range and we have to set V
                //
                // if the original accumulator and the subtracted value do share the same sign bit, then they
                // cannot overflow the signed bounds
                if (acc ^ val) & 0x80 != 0 && (acc ^ self.a) & 0x80 != 0 {
                    self.flags |= flags::OVERFLOW;
                } else {
                    self.flags &= !flags::OVERFLOW;
                }
            }
            Or(am) => {
                let val = am.get_value_and_adjust_pc(self);
                let r = self.a | val;
                self.update_nz_flags(r);
                self.a = r;
            }
            ExclusiveOr(am) => {
                let val = am.get_value_and_adjust_pc(self);
                let r = self.a ^ val;
                self.update_nz_flags(r);
                self.a = r;
            }
            RotateAccumulatorRight => {
                let curr_c = self.flags & 1;
                if self.a & 1 > 0 {
                    self.flags |= flags::CARRY;
                } else {
                    self.flags &= !flags::CARRY;
                }

                self.a = if curr_c > 0 {
                    (self.a >> 1) | 0x80
                } else {
                    self.a >> 1
                };
                self.update_nz_flags(self.a);
                self.pc += 1;
            }
            RotateRight(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                let val = self.memory.read_u8(addr);

                let curr_c = self.flags & 1;
                if val & 1 > 0 {
                    self.flags |= flags::CARRY;
                } else {
                    self.flags &= !flags::CARRY;
                }

                let new_val = if curr_c > 0 {
                    (val >> 1) | 0x80
                } else {
                    val >> 1
                };
                self.update_nz_flags(new_val);
                self.memory.write_u8(addr, new_val);
            }
            RotateAccumulatorLeft => {
                let curr_c = self.flags & 1;
                if self.a & 0x80 > 0 {
                    self.flags |= flags::CARRY;
                } else {
                    self.flags &= !flags::CARRY;
                }

                self.a = if curr_c > 0 {
                    (self.a << 1) | 1
                } else {
                    self.a << 1
                };
                self.update_nz_flags(self.a);
                self.pc += 1;
            }
            RotateLeft(am) => {
                let addr = am.get_target_addr_and_adjust_pc(self);
                let val = self.memory.read_u8(addr);

                let curr_c = self.flags & 1;
                if val & 0x80 > 0 {
                    self.flags |= flags::CARRY;
                } else {
                    self.flags &= !flags::CARRY;
                }

                let new_val = if curr_c > 0 {
                    (val << 1) | 1
                } else {
                    val << 1
                };
                self.update_nz_flags(new_val);
                self.memory.write_u8(addr, new_val);
            }
            TestBits(am) => {
                let val = am.get_value_and_adjust_pc(self);

                if val & flags::NEGATIVE > 0 {
                    self.flags |= flags::NEGATIVE;
                } else {
                    self.flags &= !flags::NEGATIVE;
                }

                if val & flags::OVERFLOW > 0 {
                    self.flags |= flags::OVERFLOW;
                } else {
                    self.flags &= !flags::OVERFLOW;
                }

                if val & self.a == 0 {
                    self.flags |= flags::ZERO;
                } else {
                    self.flags &= !flags::ZERO;
                }
            }
            ForceBreak => {
                self.flags |= flags::INTERRUPT_DISABLE;

                // get PC for jump from NMI vector
                let new_pc = self.memory.read_u16(NMI_VECTOR);

                // store return PC on stack
                let return_pc = self.current_instruction.address + 2;
                self.push((return_pc >> 8) as u8);
                self.push(return_pc as u8);

                // store status register on stack
                self.push(self.sp | flags::B);

                self.pc = new_pc;
            }
            ReturnFromInterrupt => {
                self.flags = self.pop() & 0b11001111;

                let lo = self.pop();
                let hi = self.pop();

                let new_pc = ((hi as u16) << 8 | lo as u16);
                self.pc = new_pc;
            }
            NoOp => {
                self.pc += 1;
            }
            Initial => unreachable!(),
        }
    }
}
