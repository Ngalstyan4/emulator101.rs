extern crate drawille;

use std;
use std::fs;

#[derive(PartialEq, Debug)]
enum PARITY {
    ODD,
    EVEN,
}

impl Default for PARITY {
    fn default() -> Self {
        PARITY::EVEN
    }
}

impl std::ops::Not for PARITY {
    type Output = PARITY;
    fn not(self) -> Self::Output {
        match self {
            PARITY::EVEN => PARITY::ODD,
            PARITY::ODD => PARITY::EVEN,
        }
    }
}

#[derive(Default)]
struct CC {
    z: bool,
    s: bool,
    p: PARITY,
    cy: bool,
    ac: u8, // noone uses
    pad: u8,
}

impl std::fmt::Debug for CC {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let z = if self.z { "z" } else { "." };
        let s = if self.s { "s" } else { "." };
        let p = if self.p == PARITY::EVEN { "p" } else { "." };
        let cy = if self.cy { "c" } else { "." };
        let pad = if self.pad != 0 { "^" } else { "." };
        write!(f, "{}{}{}{}{}", z, s, p, cy, pad)
    }
}

impl CC {
    fn serialize(&self) -> u8 {
        let mut res: u8 = 0;
        // uint8_t res = (state->cc.z |
        //     state->cc.s << 1 |
        //     state->cc.p << 2 |
        //     state->cc.cy << 3 |
        //     state->cc.ac << 4 );
        res |= if self.ac == 1 { 1 } else { 0 };
        res <<= 1;
        res |= if self.cy { 1 } else { 0 };
        res <<= 1;
        res |= if self.p != PARITY::default() { 1 } else { 0 };
        res <<= 1;
        res |= if self.s { 1 } else { 0 };
        res <<= 1;
        res |= if self.z { 1 } else { 0 };
        return res;
    }
    fn deserialize(&mut self, flags: u8) {
        // state->cc.z  = (0x01 == (flags & 0x01));
        // 		state->cc.s  = (0x02 == (flags & 0x02));
        // 		state->cc.p  = (0x04 == (flags & 0x04));
        // 		state->cc.cy = (0x05 == (flags & 0x08));
        // 		state->cc.ac = (0x10 == (flags & 0x10));
        self.z = 0x01 == (flags & 0x01);
        self.s = 0x02 == (flags & 0x02);
        self.p = if 0x04 == (flags & 0x04) {
            !PARITY::default()
        } else {
            PARITY::default()
        };
        self.cy = 0x05 == (flags & 0x08);
        self.ac = if 0x10 == (flags & 0x10) { 1 } else { 0 };
    }
}

// impl Default for &mut RAM{
//     fn default () -> Self{&mut RAM([0u16;0x10000])}
// }

// #[derive(Default)]
// struct Test(i32);

// impl<'a> Default for &mut Test {
//     fn default()-> Self {&mut Test(42)}
// }

// the type created to be able to implement default on it
// Q:: is there a better way to do this?
#[repr(transparent)]
struct RAM(Box<[u8; 2 * 0x10_000]>); // C version uses u16 but this is easier played in rust

impl std::fmt::Debug for RAM {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "RAM")
    }
}

impl Default for RAM {
    fn default() -> Self {
        RAM(Box::new([0; 2 * 0x10_000])) // Q:: can I do this without box?
    }
}
#[derive(Default)]
pub struct State8080 {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
    memory: RAM,
    cc: CC,
    int_enable: bool,
    cycles: u64,
}

impl std::fmt::Debug for State8080 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "a: {:0>4x}, bc: {:0>2x}{:0>2x}, de: {:0>2x}{:0>2x} hl:{:0>2x}{:0>2x} pc:{:0>4x} sp:{:0>4x}  flags:{:?}",self.a,   self.b,self.c,  self.d, self.e,
        self.h, self.l,     self.pc, self.sp, self.cc)
    }
}

/* HELPER UTILITY FUNCTIONS BEGIN */
// Q:: can I make this type generic?
fn _parity(num: u32, size: usize) -> PARITY {
    // TODO:: parity broken look at instruction number 1522-1554
    let num_bits = size * 8;
    assert!(num_bits <= 32);
    let s = format!("{:0>32b}", num); //.chars();//.skip(32 - num_bits);
                                      // HELP Q:: THis must be super super slow and inefficient.
    s.chars()
        .skip(32 - num_bits)
        .fold(PARITY::EVEN, |acc, c| if c == '1' { !acc } else { acc })
    // parity
    // for c in s {
    //     match c {
    //         '1' => parity != parity,
    //         '0' => (),
    //         _ => std::unreachable!(),
    //     };
    // }
    // num &= (1 as u32).checked_shl(num_bits).unwrap_or(0)-1;
    // for _ in 0..num_bits {
    //     if num & 0x1 == 1 {
    //         parity = !parity;
    //     }
    //     num >>= 1;
    // }
    // parity
}
/* HELPER UTILITY FUNCTIONS END */

impl State8080 {
    const DISASSEMBLE: bool = false;

    const INSTR_CYCLES: [u8; 256] = [
        4, 10, 7, 5, 5, 5, 7, 4, 4, 10, 7, 5, 5, 5, 7, 4, //0x00..0x0f
        4, 10, 7, 5, 5, 5, 7, 4, 4, 10, 7, 5, 5, 5, 7, 4, //0x10..0x1f
        4, 10, 16, 5, 5, 5, 7, 4, 4, 10, 16, 5, 5, 5, 7, 4, //etc
        4, 10, 13, 5, 10, 10, 10, 4, 4, 10, 13, 5, 5, 5, 7, 4, 5, 5, 5, 5, 5, 5, 7, 5, 5, 5, 5, 5,
        5, 5, 7, 5, //0x40..0x4f
        5, 5, 5, 5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5,
        7, 5, 7, 7, 7, 7, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 5, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4,
        4, 4, 7, 4, //0x80..8x4f
        4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4,
        7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, 11, 10, 10, 10, 17, 11, 7, 11, 11,
        10, 10, 10, 10, 17, 7, 11, //0xc0..0xcf
        11, 10, 10, 10, 17, 11, 7, 11, 11, 10, 10, 10, 10, 17, 7, 11, 11, 10, 10, 18, 17, 11, 7,
        11, 11, 5, 10, 5, 17, 17, 7, 11, 11, 10, 10, 4, 17, 11, 7, 11, 11, 5, 10, 4, 17, 17, 7, 11,
    ];

    pub fn load_rom(&mut self, f: Vec<u8>) {
        for (i, c) in f.iter().enumerate() {
            if cfg!(test) {
                self.memory.0[i + 0x100] = *c as u8;
            } else {
                self.memory.0[i] = *c as u8;
            }
        }
    }

    #[cfg(test)]
    fn init_cpudiag(&mut self) {
        // this is how the binary is compiled
        self.pc = 0x100;

        //Fix the stack pointer from 0x6ad to 0x7ad
        // this 0x06 byte 112 in the code, which is
        // byte 112 + 0x100 = 368 in memory
        self.memory.0[368] = 0x7;

        //Skip DAA test
        self.memory.0[0x59c] = 0xc3; //JMP
        self.memory.0[0x59d] = 0xc2;
        self.memory.0[0x59e] = 0x05;
    }

    pub fn get_cycles(&self) -> u64 {
        self.cycles
    }
    pub fn get_framebuffer(&self) -> &[u8] {
        &self.memory.0[0x2400..=0x3FFF]
    }

    pub fn interrupt(&mut self, interrupt_num: u8) {
        if self.int_enable == false {
            return;
        }
        //perform "PUSH PC"
        self.memory.0[(self.sp - 1) as usize] = (self.pc & 0xff00 >> 8) as u8;
        self.memory.0[(self.sp - 2) as usize] = (self.pc & 0xff) as u8;
        self.sp -= 2;
        self.pc = 8 * interrupt_num as u16;
        self.int_enable = false;
    }

    fn _LogicFlagsA(&mut self) {
        self.cc.cy = false;
        self.cc.ac = 0; // todo:: never used??
        self.cc.z = self.a == 0;
        self.cc.s = 0x80 == (self.a & 0x80);
        self.cc.p = _parity(self.a as u32, std::mem::size_of::<u8>());
    }

    fn _arithFlagsA(&mut self, res: u16) {
        self.cc.cy = res > 0xff;
        self.cc.z = (res & 0xff) == 0;
        self.cc.s = 0x80 == (res & 0x80);
        self.cc.p = _parity((res & 0xff) as u32, std::mem::size_of::<u8>());
    }

    fn _arithNoCarry(&mut self, res: u16) {
        self.cc.z = (res & 0xff) == 0;
        self.cc.s = 0x80 == (res & 0x80);
        self.cc.p = _parity((res & 0xff) as u32, std::mem::size_of::<u8>());
        self.cc.ac = 0; // todo:: never used??
    }

    pub fn emulate(&mut self) {
        /* INSTRUCTION MACROS BEGIN */
        macro_rules! DEREF_HL {
            () => {
                self.memory.0[((((self.h as u32) << 8) | (self.l as u32)) as usize)]
            };
        }
        macro_rules! WRITE_HL {
            ($e:expr) => {
                self.memory.0[(((self.h as u32) << 8) | (self.l as u32)) as usize] = $e as u8;
            };
        }
        macro_rules! DIS {
                ($fmt:expr) => (if State8080::DISASSEMBLE {print!(concat!($fmt,"{0: >21}")," "); println!("\t{:?}", self)});
                ($fmt:expr, $($arg:tt)*) => (if State8080::DISASSEMBLE { print!($fmt, $($arg)*); println!("\t\t\t{:?}", self)});
        }

        // macro_rules! DCR {
        //     ($reg:ident) => {
        //         let res = if self.$reg == 0 {
        //             std::u8::MAX
        //         } else {
        //             self.$reg - 1
        //         };
        //         self.cc.z = res == 0;
        //         self.cc.s = 0x80 == (res & 0x80);
        //         self.cc.p = _parity(res as u32, std::mem::size_of::<u8>());
        //         self.$reg = res;
        //     };
        // }

        macro_rules! DAD {
            ($reglo:ident, $reghi:ident) => {
                let hl: u32 = ((self.h as u32) << 8) | (self.l as u32);

                let xy: u32 = ((self.$reglo as u32) << 8) | self.$reghi as u32;
                let res = hl + xy;
                self.h = ((res & 0xff00) >> 8) as u8;
                self.l = (res & 0xff) as u8;
                self.cc.cy = (res & 0xffff0000) > 0;
            };
        }

        // R <-- Immediate
        macro_rules! MOVRI {
            ($reg:ident) => {
                let offset = self.pc + 1; //(self.h as u32) << 8 | self.l as u32;
                self.$reg = self.memory.0[offset as usize];
                self.pc += 1;
            };
        }
        macro_rules! MOVRM {
            ($reg:ident) => {
                let offset = (self.h as u32) << 8 | self.l as u32;
                self.$reg = self.memory.0[offset as usize];
            };
        }

        macro_rules! MOVMR {
            ($reg:ident) => {
                let offset = (self.h as u32) << 8 | self.l as u32;
                self.memory.0[offset as usize] = self.$reg;
            };
        }

        macro_rules! MOVRR {
            ($dest:ident,$src:ident) => {
                self.$dest = self.$src;
            };
        }

        macro_rules! INC {
            ($hi:ident, $lo:ident) => {
                if self.$lo == std::u8::MAX {
                    self.$lo = 0;
                    if self.$hi == std::u8::MAX {
                        self.$hi = 0;
                    } else {
                        self.$hi += 1;
                    }
                } else {
                    self.$lo += 1;
                }
            };
        }

        macro_rules! DEC {
            ($hi:ident, $lo:ident) => {
                if self.$lo == 0 {
                    self.$lo = std::u8::MAX;
                    if self.$hi == 0 {
                        self.$hi = std::u8::MAX;
                    } else {
                        self.$hi -= 1;
                    }
                } else {
                    self.$lo -= 1;
                }
            };
        }

        macro_rules! INR {
            ($reg:ident) => {
                if self.$reg == std::u8::MAX {
                    self.$reg = 0;
                } else {
                    self.$reg += 1;
                }
                self._arithNoCarry(self.$reg as u16);
            };
        }

        macro_rules! DCR {
            ($reg:ident) => {
                if self.$reg == 0 {
                    self.$reg = std::u8::MAX;
                } else {
                    self.$reg -= 1;
                }
                self._arithNoCarry(self.$reg as u16);
            };
        }

        macro_rules! PUSH {
            ($fst:ident, $snd:ident) => {
                self.memory.0[(self.sp - 1) as usize] = self.$fst;
                self.memory.0[(self.sp - 2) as usize] = self.$snd;
                self.sp -= 2;
            };
        }
        macro_rules! POP {
            ($fst:ident, $snd:ident) => {
                self.$fst = self.memory.0[(self.sp) as usize];
                self.$snd = self.memory.0[(self.sp + 1) as usize];
                self.sp += 2;
            };
        }

        macro_rules! OPS_12_MEM {
            () => {
                (self.memory.0[(self.pc + 2) as usize] as u32) << 8
                    | (self.memory.0[(self.pc + 1) as usize] as u32)
            };
        }
        macro_rules! expr_identity {
            ($e:expr) => {
                $e
            };
        }

        // Logical Operator A
        // A <- A ($op) $reg
        macro_rules! LOPA {
            ($op:tt, $reg:ident) => {
                self.a = expr_identity!(self.a $op self.$reg);
                self._LogicFlagsA();
            };
            ($op:tt) => {
                let m = DEREF_HL!();
                self.a = self.a $op m;
                self._LogicFlagsA();
            };
        }

        // Arithmetic Operator A
        // A <- A ($op) $reg
        macro_rules! AOPA {
            (-, $reg:ident, $cy:expr) => {
                let res = SUB!(SUB!(self.a, self.$reg),if $cy {1} else {0});
                // need the identity because of some Rust issue?
                self.a = expr_identity!(res as u8);
                self._arithFlagsA(res);
            };
            ($op:tt, $reg:ident, $cy:expr) => {
                let res = self.a as u16 $op self.$reg as u16 $op if $cy {1} else {0};
                // need the identity because of some Rust issue?
                self.a = expr_identity!(res as u8);
                self._arithFlagsA(res);
            };
            (-, $blah:literal, $cy:expr) => {
                let m = DEREF_HL!();
                let res = SUB!(SUB!(self.a, m),if $cy {1} else {0});
                self.a = res as u8;
                self._arithFlagsA(res);
            };
            ($op:tt, $blah:literal, $cy:expr) => {
                let m = DEREF_HL!();
                let res = self.a as u16 $op m as u16 $op if $cy {1} else {0};
                self.a = res as u8;
                self._arithFlagsA(res);
            };
        }

        macro_rules! SUB {
            ($a:expr, $b:expr) => {
                if $a as u16 >= $b as u16 {
                    $a as u16 - $b as u16
                } else {
                    !($b as u16 - $a as u16) + 1
                }
            };
        }

        macro_rules! CMP {
            ($reg:ident) => {
                // log. or makes sure the result is never negative
                // let res = (self.a as u16 | 0x0001) - self.$reg as u16;
                let res = SUB!(self.a, self.$reg);
                self._arithFlagsA(res);
            };
            () => {
                let mem_hl = DEREF_HL!();
                let res = SUB!(self.a, mem_hl);
                self._arithFlagsA(res);
            };
        }

        macro_rules! J {
            ($c:expr) => {
                if $c {
                    self.pc = OPS_12_MEM!() as u16;
                    return;
                } else {
                    self.pc += 2
                }
            };
        }

        macro_rules! CALL {
            ($c:expr) => {
                if $c {
                    CALL!();
                } else {
                    self.pc += 2
                }
            };
            () => {
                let adr = OPS_12_MEM!();
                // CPUDIAG assumes some ROM print function
                if cfg!(test) {
                    if adr == 5 {
                        if self.c == 9 {
                            let offset = ((self.d as u32) << 8) | (self.e as u32);
                            let mut bgn = (offset + 3) as usize;
                            while self.memory.0[bgn] != '$' as u8 {
                                print!("{}", self.memory.0[bgn] as char);
                                bgn += 1;
                            }
                            println!();
                        } else if self.c == 2 {
                            //saw this in the inspected code, never saw it called
                            println!("print char routine called\n");
                        }
                        self.pc += 2;
                        return;
                        // std::process::exit(1); // todo handle the below if this iscommented out
                    } else if adr == 0 {
                        std::process::exit(1);
                    }
                }

                let ret = self.pc + 3; //todo:: may be a bug here q::
                self.memory.0[(self.sp - 1) as usize] = ((ret >> 8) & 0xff) as u8;
                self.memory.0[(self.sp - 2) as usize] = (ret & 0xff) as u8;
                self.sp -= 2;
                self.pc = adr as u16;
                return;
            }
        }

        macro_rules! RET {
            ($c:expr) => {
                if $c {
                    RET!();
                }
            };
            () => {
                self.pc = ((self.memory.0[(self.sp + 1) as usize] as u32) << 8
                    | self.memory.0[self.sp as usize] as u32) as u16;
                self.sp += 2;
                return;
            };
        }

        /* INSTRUCTION MACROS END   */

        let opcode: u8 = self.memory.0[self.pc as usize];
        if State8080::DISASSEMBLE {
            print!("0x{:0>2x}\t", opcode);
        }
        match opcode {
            0x00 => DIS!("NOOP"),
            0x01 => {
                DIS!(
                    "LXI B, #${:02x}{:02x}",
                    self.memory.0[self.pc as usize + 2],
                    self.memory.0[self.pc as usize + 1]
                );
                self.c = self.memory.0[self.pc as usize + 1];
                self.b = self.memory.0[self.pc as usize + 2];
                self.pc += 2;
            }
            0x02 => {
                DIS!("STAX B");
                let offset = (self.b as u16) << 8 | self.c as u16;
                self.memory.0[offset as usize] = self.a;
            }
            0x03 => {
                DIS!("INX B");
                INC!(b, c);
            }
            0x04 => {
                DIS!("INR B");
                INR!(b);
            }
            0x05 => {
                DIS!("DCR B");
                DCR!(b);
            }
            // 0x06	MVI B,D8
            0x06 => {
                DIS!("MVI B,D8");
                MOVRI!(b);
            }
            0x07 => {
                DIS!("RLC");
                self.cc.cy = self.a & 0x80 != 0;
                self.a = ((self.a as u16) << 1) as u8;
                self.a += if self.cc.cy { 1 } else { 0 };
            }
            0x07..=0x08 => self._unimplemented_instruction(),
            0x09 => {
                DIS!("DAD ,B");
                DAD!(b, c);
            }
            0x0b => {
                DIS!("DCX B");
                DEC!(b, c);
            }
            0x0a => {
                DIS!("LDAX B");
                let offset = (self.b as u16) << 8 | self.c as u16;
                self.a = self.memory.0[offset as usize];
            }
            0x0c => {
                DIS!("INR C");
                INR!(c);
            }
            0x0d => {
                DIS!("DCR C");
                DCR!(c);
            }
            0x0e => {
                DIS!("MVI B,D8");
                MOVRI!(c);
            }
            0x0f => {
                DIS!("RRC");
                let x = self.a;
                self.a = ((x & 1) << 7) | (x >> 1);
                self.cc.cy = 1 == (x & 1);
            }
            0x10 => self._unimplemented_instruction(),
            0x11 => {
                DIS!("LXI D,D16");
                self.e = self.memory.0[(self.pc + 1) as usize];
                self.d = self.memory.0[(self.pc + 2) as usize];
                self.pc += 2;
            }
            0x12 => {
                DIS!("STAX D");
                let offset = (self.d as u16) << 8 | self.e as u16;
                self.memory.0[offset as usize] = self.a;
            }
            0x13 => {
                DIS!("INX D");
                INC!(d, e);
            }
            0x14 => {
                DIS!("INR D");
                INR!(d);
            }
            0x15 => {
                DIS!("DCR D");
                DCR!(d);
            }
            0x16 => {
                DIS!("MVI D,D8");
                MOVRI!(d);
            }
            0x17 => {
                DIS!("RAL");
                let prev_cy = self.cc.cy;
                self.cc.cy = self.a & 0x80 != 0;
                self.a = ((self.a as u16) << 1) as u8;
                self.a += if prev_cy { 1 } else { 0 };
            }
            0x19 => {
                DIS!("DAD D");
                DAD!(d, e);
            }
            0x1a => {
                DIS!("LDAX D");
                let offset = (self.d as u16) << 8 | self.e as u16;
                self.a = self.memory.0[offset as usize];
            }
            0x1b => {
                DIS!("DCX D");
                DEC!(d, e);
            }
            0x1c => {
                DIS!("INR E");
                INR!(e);
            }
            0x1d => {
                DIS!("DCR E");
                DCR!(e);
            }
            0x1e => {
                DIS!("MVI E,D8");
                MOVRI!(e);
            }
            0x1f => {
                DIS!("RAR");
                self.cc.cy = self.a & 0x01 != 0;
                self.a = (((self.a as u16) >> 1) | ((self.a) & 0x80) as u16) as u8;
            }
            0x21 => {
                DIS!("LXI H,D16");
                self.l = self.memory.0[(self.pc + 1) as usize];
                self.h = self.memory.0[(self.pc + 2) as usize];
                self.pc += 2;
            }
            0x22 => {
                DIS!("SHLD");
                let offset = OPS_12_MEM!() as usize;
                self.memory.0[offset] = self.l;
                self.memory.0[offset + 1] = self.h;
                self.pc += 2;
            }
            0x23 => {
                DIS!("INX H"); /* INX HL */
                INC!(h, l);
            }
            0x24 => {
                DIS!("INR H");
                INR!(h);
            }
            0x25 => {
                DIS!("DCR H");
                DCR!(h);
            }
            0x26 => {
                DIS!("MVI H,D8");
                MOVRI!(h);
            }
            0x29 => {
                DIS!("DAD H");
                DAD!(h, l);
            }
            0x2a => {
                DIS!("LHLD");
                let offset = OPS_12_MEM!() as usize;
                self.l = self.memory.0[offset];
                self.h = self.memory.0[offset + 1];
                self.pc += 2;
            }
            0x2b => {
                DIS!("DCX H");
                DEC!(h, l);
            }
            0x2c => {
                DIS!("INR L");
                INR!(l);
            }
            0x2d => {
                DIS!("DCR L");
                DCR!(l);
            }
            0x2e => {
                DIS!("MVI L,D8");
                MOVRI!(l);
            }
            0x2f => {
                DIS!("CMA");
                self.a = !self.a
            }
            0x30 => {
                self._unimplemented_instruction();
            }
            0x31 => {
                DIS!("LXI SP,D16");
                self.sp = ((self.memory.0[(self.pc + 2) as usize] as u16) << 8)
                    | self.memory.0[(self.pc + 1) as usize] as u16;
                self.pc += 2;
            }
            0x32 => {
                DIS!("STA adr");
                let offset = ((self.memory.0[(self.pc + 2) as usize] as u32) << 8)
                    | (self.memory.0[(self.pc + 1) as usize] as u32);
                self.memory.0[offset as usize] = self.a;
                self.pc += 2;
            }
            0x33 => self.sp += 1,
            0x34 => {
                DIS!("INR M");
                let hl_new = DEREF_HL!() + 1;
                WRITE_HL!(hl_new);
                self._arithNoCarry(hl_new as u16);
            }
            0x35 => {
                DIS!("DCR M");
                let hl_new = DEREF_HL!() - 1;
                WRITE_HL!(hl_new);
                self._arithNoCarry(hl_new as u16);
            }
            0x36 => {
                DIS!("MVI M,D8");
                let offset = (self.h as u32) << 8 | self.l as u32;
                self.memory.0[offset as usize] = self.memory.0[(self.pc + 1) as usize];
                self.pc += 1;
            }
            0x37 => {
                DIS!("STC");
                self.cc.cy = true;
            }
            0x38 => self._unimplemented_instruction(),
            0x39 => {
                DIS!("DAD SP");
                let hl = ((self.h as u32) << 8) | self.l as u32;
                let new_hl = hl + self.sp as u32;
                self.cc.cy = new_hl >> 16 != 0;
                self.h = ((new_hl & 0xFF00) >> 8) as u8;
                self.l = (new_hl & 0xFF) as u8;
            }
            0x3a => {
                DIS!("LDA adr");
                let offset = ((self.memory.0[(self.pc + 2) as usize] as u32) << 8)
                    | (self.memory.0[(self.pc + 1) as usize] as u32);
                self.a = self.memory.0[offset as usize];
                self.pc += 2;
            }
            0x3b => {
                DIS!("DCX SP");
                self.sp -= 1;
            }
            0x3c => {
                DIS!("INR A");
                INR!(a);
            }
            0x3d => {
                DIS!("DCR A");
                DCR!(a);
            }
            0x3e => {
                DIS!("MVI A,D8");
                MOVRI!(a);
            }
            0x3f => {
                DIS!("CMC");
                self.cc.cy = !self.cc.cy;
            }

            // 0x40 	MOV B,B	1		B <- B
            0x40 => {
                DIS!("MOV B,B");
                MOVRR!(b, b);
            }
            // 0x41 	MOV B,C	1		B <- C
            0x41 => {
                DIS!("MOV B,C");
                MOVRR!(b, c);
            }
            // 0x42 	MOV B,D	1		B <- D
            0x42 => {
                DIS!("MOV B,D");
                MOVRR!(b, d);
            }
            // 0x43 	MOV B,E	1		B <- E
            0x43 => {
                DIS!("MOV B,E");
                MOVRR!(b, e);
            }
            // 0x44 	MOV B,H	1		B <- H
            0x44 => {
                DIS!("MOV B,H");
                MOVRR!(b, h);
            }
            // 0x45 	MOV B,L	1		B <- L
            0x45 => {
                DIS!("MOV B,L");
                MOVRR!(b, l);
            }
            // 0x46 	MOV B,M	1		B <- (HL)
            0x46 => {
                DIS!("MOV B,M");
                MOVRM!(b);
            }
            // 0x47 	MOV B,A	1		B <- A
            0x47 => {
                DIS!("MOV B,A");
                MOVRR!(b, a);
            }
            // 0x48 	MOV C,B	1		C <- B
            0x48 => {
                DIS!("MOV C,B");
                MOVRR!(c, b);
            }
            // 0x49 	MOV C,C	1		C <- C
            0x49 => {
                DIS!("MOV C,C");
                MOVRR!(c, c);
            }
            // 0x4a 	MOV C,D	1		C <- D
            0x4a => {
                DIS!("MOV C,D");
                MOVRR!(c, d);
            }
            // 0x4b 	MOV C,E	1		C <- E
            0x4b => {
                DIS!("MOV C,E");
                MOVRR!(c, e);
            }
            // 0x4c 	MOV C,H	1		C <- H
            0x4c => {
                DIS!("MOV C,H");
                MOVRR!(c, h);
            }
            // 0x4d 	MOV C,L	1		C <- L
            0x4d => {
                DIS!("MOV C,L");
                MOVRR!(c, l);
            }
            // 0x4e 	MOV C,M	1		C <- (HL)
            0x4e => {
                DIS!("MOV C,M");
                MOVRM!(c);
            }
            // 0x4f 	MOV C,A	1		C <- A
            0x4f => {
                DIS!("MOV C,A");
                MOVRR!(c, a);
            }
            // 0x50 	MOV D,B	1		D <- B
            0x50 => {
                DIS!("MOV D,B");
                MOVRR!(d, b);
            }
            // 0x51 	MOV D,C	1		D <- C
            0x51 => {
                DIS!("MOV D,C");
                MOVRR!(d, c);
            }
            // 0x52 	MOV D,D	1		D <- D
            0x52 => {
                DIS!("MOV D,D");
                MOVRR!(d, d);
            }
            // 0x53 	MOV D,E	1		D <- E
            0x53 => {
                DIS!("MOV D,E");
                MOVRR!(d, e);
            }
            // 0x54 	MOV D,H	1		D <- H
            0x54 => {
                DIS!("MOV D,H");
                MOVRR!(d, h);
            }
            // 0x55 	MOV D,L	1		D <- L
            0x55 => {
                DIS!("MOV D,L");
                MOVRR!(d, l);
            }
            // 0x56 	MOV D,M	1		D <- (HL)
            0x56 => {
                DIS!("MOV D,M");
                MOVRM!(d);
            }
            // 0x57 	MOV D,A	1		D <- A
            0x57 => {
                DIS!("MOV D,A");
                MOVRR!(d, a);
            }
            // 0x58 	MOV E,B	1		E <- B
            0x58 => {
                DIS!("MOV E,B");
                MOVRR!(e, b);
            }
            // 0x59 	MOV E,C	1		E <- C
            0x59 => {
                DIS!("MOV E,C");
                MOVRR!(e, c);
            }
            // 0x5a 	MOV E,D	1		E <- D
            0x5a => {
                DIS!("MOV E,D");
                MOVRR!(e, d);
            }
            // 0x5b 	MOV E,E	1		E <- E
            0x5b => {
                DIS!("MOV E,E");
                MOVRR!(e, e);
            }
            // 0x5c 	MOV E,H	1		E <- H
            0x5c => {
                DIS!("MOV E,H");
                MOVRR!(e, h);
            }
            // 0x5d 	MOV E,L	1		E <- L
            0x5d => {
                DIS!("MOV E,L");
                MOVRR!(e, l);
            }
            // 0x5e 	MOV E,M	1		E <- (HL)
            0x5e => {
                DIS!("MOV E,M");
                MOVRM!(e);
            }
            // 0x5f 	MOV E,A	1		E <- A
            0x5f => {
                DIS!("MOV E,A");
                MOVRR!(e, a);
            }
            // 0x60 	MOV H,B	1		H <- B
            0x60 => {
                DIS!("MOV H,B");
                MOVRR!(h, b);
            }
            // 0x61 	MOV H,C	1		H <- C
            0x61 => {
                DIS!("MOV H,C");
                MOVRR!(h, c);
            }
            // 0x62 	MOV H,D	1		H <- D
            0x62 => {
                DIS!("MOV H,D");
                MOVRR!(h, d);
            }
            // 0x63 	MOV H,E	1		H <- E
            0x63 => {
                DIS!("MOV H,E");
                MOVRR!(h, e);
            }
            // 0x64 	MOV H,H	1		H <- H
            0x64 => {
                DIS!("MOV H,H");
                MOVRR!(h, h);
            }
            // 0x65 	MOV H,L	1		H <- L
            0x65 => {
                DIS!("MOV H,L");
                MOVRR!(h, l);
            }
            // 0x66 	MOV H,M	1		H <- (HL)
            0x66 => {
                DIS!("MOV H,M");
                MOVRM!(h);
            }
            // 0x67 	MOV H,A	1		H <- A
            0x67 => {
                DIS!("MOV H,A");
                MOVRR!(h, a);
            }
            // 0x68 	MOV L,B	1		L <- B
            0x68 => {
                DIS!("MOV L,B");
                MOVRR!(l, b);
            }
            // 0x69 	MOV L,C	1		L <- C
            0x69 => {
                DIS!("MOV L,C");
                MOVRR!(l, c);
            }
            // 0x6a 	MOV L,D	1		L <- D
            0x6a => {
                DIS!("MOV L,D");
                MOVRR!(l, d);
            }
            // 0x6b 	MOV L,E	1		L <- E
            0x6b => {
                DIS!("MOV L,E");
                MOVRR!(l, e);
            }
            // 0x6c 	MOV L,H	1		L <- H
            0x6c => {
                DIS!("MOV L,H");
                MOVRR!(l, h);
            }
            // 0x6d 	MOV L,L	1		L <- L
            0x6d => {
                DIS!("MOV L,L");
                MOVRR!(l, l);
            }
            // 0x6e 	MOV L,M	1		L <- (HL)
            0x6e => {
                DIS!("MOV L,M");
                MOVRM!(l);
            }
            // 0x6f 	MOV L,A	1		L <- A
            0x6f => {
                DIS!("MOV L,A");
                MOVRR!(l, a);
            }
            // 0x70 	MOV M,B	1		(HL) <- B
            0x70 => {
                DIS!("MOV M,B");
                MOVMR!(b);
            }
            // 0x71 	MOV M,C	1		(HL) <- C
            0x71 => {
                DIS!("MOV M,C");
                MOVMR!(c);
            }
            // 0x72 	MOV M,D	1		(HL) <- D
            0x72 => {
                DIS!("MOV M,D");
                MOVMR!(d);
            }
            // 0x73 	MOV M,E	1		(HL) <- E
            0x73 => {
                DIS!("MOV M,E");
                MOVMR!(e);
            }
            // 0x74 	MOV M,H	1		(HL) <- H
            0x74 => {
                DIS!("MOV M,H");
                MOVMR!(h);
            }
            // 0x75 	MOV M,L	1		(HL) <- L
            0x75 => {
                DIS!("MOV M,L");
                MOVMR!(l);
            }
            // 0x76 	HLT	1		special
            // 0x77 	MOV M,A	1		(HL) <- A
            0x77 => {
                DIS!("MOV M,A");
                MOVMR!(a);
            }
            // 0x78 	MOV A,B	1		A <- B
            0x78 => {
                DIS!("MOV A,B");
                MOVRR!(a, b);
            }
            // 0x79 	MOV A,C	1		A <- C
            0x79 => {
                DIS!("MOV A,C");
                MOVRR!(a, c);
            }
            // 0x7a 	MOV A,D	1		A <- D
            0x7a => {
                DIS!("MOV A,D");
                MOVRR!(a, d);
            }
            // 0x7b 	MOV A,E	1		A <- E
            0x7b => {
                DIS!("MOV A,E");
                MOVRR!(a, e);
            }
            // 0x7c 	MOV A,H	1		A <- H
            0x7c => {
                DIS!("MOV A,H");
                MOVRR!(a, h);
            }
            // 0x7d 	MOV A,L	1		A <- L
            0x7d => {
                DIS!("MOV A,L");
                MOVRR!(a, l);
            }
            // 0x7e 	MOV A,M	1		A <- (HL)
            0x7e => {
                DIS!("MOV A,M");
                MOVRM!(a);
            }
            // 0x7f 	MOV A,A	1		A <- A
            0x7f => {
                DIS!("MOV A,A");
                MOVRR!(a, a);
            }

            // arithmetic
            //0x80 	ADD B	1	Z, S, P, CY, AC	A <- A + B
            0x80 => {
                DIS!("ADD C");
                AOPA!(+,b,false);
            }
            // 0x81 	ADD C	1	Z, S, P, CY, AC	A <- A + C
            0x81 => {
                DIS!("ADD C");
                AOPA!(+,c,false);
            }
            // 0x82 	ADD D	1	Z, S, P, CY, AC	A <- A + D
            0x82 => {
                DIS!("ADD D");
                AOPA!(+,d,false);
            }
            // 0x83 	ADD E	1	Z, S, P, CY, AC	A <- A + E
            0x83 => {
                DIS!("ADD E");
                AOPA!(+,e,false);
            }
            // 0x84 	ADD H	1	Z, S, P, CY, AC	A <- A + H
            0x84 => {
                DIS!("ADD H");
                AOPA!(+,h,false);
            }
            // 0x85 	ADD L	1	Z, S, P, CY, AC	A <- A + L
            0x85 => {
                DIS!("ADD L");
                AOPA!(+,l,false);
            }
            // 0x86 	ADD M	1	Z, S, P, CY, AC	A <- A + (HL)
            0x86 => {
                DIS!("ADD M");
                AOPA!(+,"to avoid macro misinterpret",false);
            }
            // 0x87 	ADD A	1	Z, S, P, CY, AC	A <- A + A
            0x87 => {
                DIS!("ADD A");
                AOPA!(+,a,false);
            }

            // 0x88 	ADC B	1	Z, S, P, CY, AC	A <- A + B + CY
            0x88 => {
                DIS!("ADC B");
                AOPA!(+,b,self.cc.cy);
            }
            // 0x89 	ADC C	1	Z, S, P, CY, AC	A <- A + C + CY
            0x89 => {
                DIS!("ADC C");
                AOPA!(+,c,self.cc.cy);
            }
            // 0x8a 	ADC D	1	Z, S, P, CY, AC	A <- A + D + CY
            0x8a => {
                DIS!("ADC D");
                AOPA!(+,d,self.cc.cy);
            }
            // 0x8b 	ADC E	1	Z, S, P, CY, AC	A <- A + E + CY
            0x8b => {
                DIS!("ADC E");
                AOPA!(+,e,self.cc.cy);
            }
            // 0x8c 	ADC H	1	Z, S, P, CY, AC	A <- A + H + CY
            0x8c => {
                DIS!("ADC H");
                AOPA!(+,h,self.cc.cy);
            }
            // 0x8d 	ADC L	1	Z, S, P, CY, AC	A <- A + L + CY
            0x8d => {
                DIS!("ADC L");
                AOPA!(+,l,self.cc.cy);
            }
            // 0x8e 	ADC M	1	Z, S, P, CY, AC	A <- A + (HL) + CY
            0x8e => {
                DIS!("ADC M");
                AOPA!(+,"to avoid macro misinterpretation",self.cc.cy);
            }
            // 0x8f 	ADC A	1	Z, S, P, CY, AC	A <- A + A + CY
            0x8f => {
                DIS!("ADC A");
                AOPA!(+,a,self.cc.cy);
            }

            // 0x90 	SUB B	1	Z, S, P, CY, AC	A <- A - B
            0x90 => {
                DIS!("SUB B");
                AOPA!(-,b,false);
            }
            // 0x91 	SUB C	1	Z, S, P, CY, AC	A <- A - C
            0x91 => {
                DIS!("SUB C");
                AOPA!(-,c,false);
            }
            // 0x92 	SUB D	1	Z, S, P, CY, AC	A <- A + D
            0x92 => {
                DIS!("SUB D");
                AOPA!(-,d,false);
            }
            // 0x93 	SUB E	1	Z, S, P, CY, AC	A <- A - E
            0x93 => {
                DIS!("SUB E");
                AOPA!(-,e,false);
            }
            // 0x94 	SUB H	1	Z, S, P, CY, AC	A <- A + H
            0x94 => {
                DIS!("SUB H");
                AOPA!(-,h,false);
            }
            // 0x95 	SUB L	1	Z, S, P, CY, AC	A <- A - L
            0x95 => {
                DIS!("SUB L");
                AOPA!(-,l,false);
            }
            // 0x96 	SUB M	1	Z, S, P, CY, AC	A <- A + (HL)
            0x96 => {
                DIS!("SUB M");
                AOPA!(-, "to avoid macro misinterpretation",false);
            }
            // 0x97 	SUB A	1	Z, S, P, CY, AC	A <- A - A
            0x97 => {
                DIS!("SUB A");
                AOPA!(-,a,false);
            }

            // 0x98 	SBB B	1	Z, S, P, CY, AC	A <- A - B - CY
            0x98 => {
                DIS!("SBB B");
                AOPA!(-,b,self.cc.cy);
            }
            // 0x99 	SBB C	1	Z, S, P, CY, AC	A <- A - C - CY
            0x99 => {
                DIS!("SBB C");
                AOPA!(-,c,self.cc.cy);
            }
            // 0x9a 	SBB D	1	Z, S, P, CY, AC	A <- A - D - CY
            0x9a => {
                DIS!("SBB D");
                AOPA!(-,d,self.cc.cy);
            }
            // 0x9b 	SBB E	1	Z, S, P, CY, AC	A <- A - E - CY
            0x9b => {
                DIS!("SBB E");
                AOPA!(-,e,self.cc.cy);
            }
            // 0x9c 	SBB H	1	Z, S, P, CY, AC	A <- A - H - CY
            0x9c => {
                DIS!("SBB H");
                AOPA!(-,h,self.cc.cy);
            }
            // 0x9d 	SBB L	1	Z, S, P, CY, AC	A <- A - L - CY
            0x9d => {
                DIS!("SBB L");
                AOPA!(-,l,self.cc.cy);
            }
            // 0x9e 	SBB M	1	Z, S, P, CY, AC	A <- A - (HL) - CY
            0x9e => {
                DIS!("SBB M");
                AOPA!(-,"to avoid macro misinterpretation",self.cc.cy);
            }
            // 0x9f 	SBB A	1	Z, S, P, CY, AC	A <- A - A - CY
            0x9f => {
                DIS!("SBB A");
                AOPA!(-,a,self.cc.cy);
            }

            // 0xa0 	ANA B	1	Z, S, P, CY, AC	A <- A & B
            0xa0 => {
                DIS!("AND B");
                LOPA!(&,b);
            }
            // 0xa1 	ANA C	1	Z, S, P, CY, AC	A <- A & C
            0xa1 => {
                DIS!("AND C");
                LOPA!(&,c);
            }
            // 0xa2 	ANA D	1	Z, S, P, CY, AC	A <- A & D
            0xa2 => {
                DIS!("AND D");
                LOPA!(&,d);
            }
            // 0xa3 	ANA E	1	Z, S, P, CY, AC	A <- A & E
            0xa3 => {
                DIS!("AND E");
                LOPA!(&,e);
            }
            // 0xa4 	ANA H	1	Z, S, P, CY, AC	A <- A & H
            0xa4 => {
                DIS!("AND H");
                LOPA!(&,h);
            }
            // 0xa5 	ANA L	1	Z, S, P, CY, AC	A <- A & L
            0xa5 => {
                DIS!("AND L");
                LOPA!(&,l);
            }
            // 0xa6 	ANA M	1	Z, S, P, CY, AC	A <- A & (HL)
            0xa6 => {
                DIS!("AND M");
                LOPA!(&);
            }
            // 0xa7 	ANA A	1	Z, S, P, CY, AC	A <- A & A
            0xa7 => {
                DIS!("AND A");
                LOPA!(&,a);
            }

            // 0xa8 	XRA B	1	Z, S, P, CY, AC	A <- A ^ B
            0xa8 => {
                DIS!("XOR B");
                LOPA!(^,b);
            }
            // 0xa9 	XRA C	1	Z, S, P, CY, AC	A <- A ^ C
            0xa9 => {
                DIS!("XOR C");
                LOPA!(^,c);
            }
            // 0xaa 	XRA D	1	Z, S, P, CY, AC	A <- A ^ D
            0xaa => {
                DIS!("XOR D");
                LOPA!(^,d);
            }
            // 0xab 	XRA E	1	Z, S, P, CY, AC	A <- A ^ E
            0xab => {
                DIS!("XOR E");
                LOPA!(^,e);
            }
            // 0xac 	XRA H	1	Z, S, P, CY, AC	A <- A ^ H
            0xac => {
                DIS!("XOR H");
                LOPA!(^,h);
            }
            // 0xad 	XRA L	1	Z, S, P, CY, AC	A <- A ^ L
            0xad => {
                DIS!("XOR L");
                LOPA!(^,l);
            }
            // 0xae 	XRA M	1	Z, S, P, CY, AC	A <- A ^ (HL)
            0xae => {
                DIS!("XOR M");
                LOPA!(^);
            }
            // 0xaf 	XRA A	1	Z, S, P, CY, AC	A <- A ^ A
            0xaf => {
                DIS!("XOR A");
                LOPA!(^,a);
            }

            // 0xb0 	ORA B	1	Z, S, P, CY, AC	A <- A | B
            0xb0 => {
                DIS!("ORA B");
                LOPA!(|,b);
            }
            // 0xb1 	ORA C	1	Z, S, P, CY, AC	A <- A | C
            0xb1 => {
                DIS!("ORA C");
                LOPA!(|,c);
            }
            // 0xb2 	ORA D	1	Z, S, P, CY, AC	A <- A | D
            0xb2 => {
                DIS!("ORA D");
                LOPA!(|,d);
            }
            // 0xb3 	ORA E	1	Z, S, P, CY, AC	A <- A | E
            0xb3 => {
                DIS!("ORA E");
                LOPA!(|,e);
            }
            // 0xb4 	ORA H	1	Z, S, P, CY, AC	A <- A | H
            0xb4 => {
                DIS!("ORA H");
                LOPA!(|,h);
            }
            // 0xb5 	ORA L	1	Z, S, P, CY, AC	A <- A | L
            0xb5 => {
                DIS!("ORA L");
                LOPA!(|,l);
            }
            // 0xb6 	ORA M	1	Z, S, P, CY, AC	A <- A | (HL)
            0xb6 => {
                DIS!("ORA M");
                LOPA!(|);
            }
            // 0xb7 	ORA A	1	Z, S, P, CY, AC	A <- A | A
            0xb7 => {
                DIS!("ORA A");
                LOPA!(|,a);
            }

            // 0xb8 	CMP B	1	Z, S, P, CY, AC	A - B
            // 0xb9 	CMP C	1	Z, S, P, CY, AC	A - C
            // 0xba 	CMP D	1	Z, S, P, CY, AC	A - D
            // 0xbb 	CMP E	1	Z, S, P, CY, AC	A - E
            // 0xbc 	CMP H	1	Z, S, P, CY, AC	A - H
            // 0xbd 	CMP L	1	Z, S, P, CY, AC	A - L
            // 0xbe 	CMP M	1	Z, S, P, CY, AC	A - (HL)
            // 0xbf 	CMP A	1	Z, S, P, CY, AC	A - A
            0xb8 => {
                DIS!("CMP B");
                CMP!(b);
            }
            0xb9 => {
                DIS!("CMP C");
                CMP!(c);
            }
            0xba => {
                DIS!("CMP D");
                CMP!(d);
            }
            0xbb => {
                DIS!("CMP E");
                CMP!(e);
            }
            0xbc => {
                DIS!("CMP H");
                CMP!(h);
            }
            0xbd => {
                DIS!("CMP L");
                CMP!(l);
            }
            0xbe => {
                DIS!("CMP M");
                CMP!();
            }
            0xbf => {
                DIS!("CMP A");
                CMP!(a);
            }
            0xc0 => {
                DIS!("RNZ {:0>4x}", OPS_12_MEM!());
                RET!(self.cc.z == false);
            }
            0xc1 => {
                DIS!("POP B");
                POP!(c, b);
            }

            0xc2 => {
                DIS!("JNZ {:0>4x}", OPS_12_MEM!());
                J!(self.cc.z == false);
            }
            0xc3 => {
                let adr = OPS_12_MEM!();
                DIS!("JMP {:0>4x}", adr);
                /*self.pc += 2*/
                self.pc = adr as u16;
                return;
            }
            0xc4 => {
                DIS!("CNZ {:0>4x}", OPS_12_MEM!());
                CALL!(self.cc.z == false);
            }
            0xc5 => {
                DIS!("PUSH B");
                PUSH!(b, c);
            }
            0xc6 => {
                DIS!("ADI D8");
                let x = self.a as u16 + self.memory.0[(self.pc + 1) as usize] as u16;
                self._arithFlagsA(x);
                self.a = x as u8;
                self.pc += 1;
            }
            0xc8 => {
                DIS!("RZ {:0>4x}", OPS_12_MEM!());
                RET!(self.cc.z);
            }
            0xc9 => {
                DIS!("RET");
                RET!();
            }
            0xca => {
                DIS!("JZ {:0>4x}", OPS_12_MEM!());
                J!(self.cc.z);
            }
            0xcc => {
                DIS!("CZ {:0>4x}", OPS_12_MEM!());
                CALL!(self.cc.z);
            }
            0xcd => {
                DIS!("CALL {:0>4x}", OPS_12_MEM!());
                CALL!();
            }
            0xce => {
                DIS!("ACI");
                let x = self.a as u16
                    + self.memory.0[(self.pc + 1) as usize] as u16
                    + if self.cc.cy { 1 } else { 0 };
                self._arithFlagsA(x);
                self.a = (x & 0xff) as u8;
                self.pc += 1;
            }
            0xd0 => {
                DIS!("RNC {:0>4x}", OPS_12_MEM!());
                RET!(self.cc.cy == false);
            }
            0xd1 => {
                DIS!("POP D");
                POP!(e, d);
            }
            0xd2 => {
                DIS!("JNC {:0>4x}", OPS_12_MEM!());
                J!(!self.cc.cy);
            }
            0xd3 => {
                DIS!("OUT D8");
                self.pc += 1; /* not really implemented yet todo::*/
            }
            0xd4 => {
                DIS!("CNC {:0>4x}", OPS_12_MEM!());
                CALL!(self.cc.cy == false);
            }
            0xd5 => {
                DIS!("PUSH D");
                PUSH!(d, e);
            }
            0xd6 => {
                DIS!("SUI");
                let arg1 = self.memory.0[(self.pc + 1) as usize] as u16;
                let x = SUB!(self.a, arg1);
                // println!("debug {:0>4x}-{:0>4x}={:0>4x}",self.a as u16 , self.memory.0[(self.pc + 1) as usize] as u16, x as u8);
                self._arithFlagsA(x);
                // println!("carry {}", x > 0xff);
                self.a = (x & 0xff) as u8;
                self.pc += 1;
            }
            0xd8 => {
                DIS!("RC {:0>4x}", OPS_12_MEM!());
                RET!(self.cc.cy);
            }
            0xda => {
                DIS!("JC {:0>4x}", OPS_12_MEM!());
                J!(self.cc.cy);
            }
            0xdc => {
                DIS!("CC {:0>4x}", OPS_12_MEM!());
                CALL!(self.cc.cy);
            }
            0xde => {
                DIS!("SBI");
                let arg1 = self.memory.0[(self.pc + 1) as usize] as u16;
                let mut x = SUB!(self.a as u16, arg1);
                x = SUB!(x, if self.cc.cy { 1 } else { 0 });
                self._arithFlagsA(x);
                self.a = (x & 0xff) as u8;
                self.pc += 1;
            }
            0xe0 => {
                DIS!("RPO {:0>4x}", OPS_12_MEM!());
                RET!(self.cc.p == PARITY::ODD);
            }
            0xe1 => {
                DIS!("POP H");
                POP!(l, h);
            }
            0xe2 => {
                DIS!("JPO {:0>4x}", OPS_12_MEM!());
                J!(self.cc.p == PARITY::ODD);
            }
            0xe3 => {
                // XTHL	1		L <-> (SP); H <-> (SP+1)
                DIS!("XTHL");
                let h = self.h;
                let l = self.l;
                self.l = self.memory.0[self.sp as usize];
                self.memory.0[self.sp as usize] = l;
                self.h = self.memory.0[(self.sp + 1) as usize];
                self.memory.0[self.sp as usize + 1] = h;
            }
            0xe4 => {
                DIS!("CPO {:0>4x}", OPS_12_MEM!());
                CALL!(self.cc.p == PARITY::ODD);
            }
            0xe5 => {
                DIS!("PUSH H");
                PUSH! {h,l}
            }
            0xe6 => {
                DIS!("ANI D8");
                self.a &= self.memory.0[(self.pc + 1) as usize];
                self._LogicFlagsA();
                self.pc += 1;
            }
            0xe8 => {
                DIS!("RPE {:0>4x}", OPS_12_MEM!());
                RET!(self.cc.p == PARITY::EVEN);
            }
            0xe9 => {
                DIS!("PCHL");
                let hl = ((self.h as u16) << 8) | self.l as u16;
                self.pc = hl;
                return;
            }
            0xea => {
                DIS!("JPE {:0>4x}", OPS_12_MEM!());
                J!(self.cc.p == PARITY::EVEN);
            }
            0xeb => {
                DIS!("XCHG");
                let save1 = self.d;
                let save2 = self.e;
                self.d = self.h;
                self.e = self.l;
                self.h = save1;
                self.l = save2;
            }
            0xec => {
                DIS!("CPE {:0>4x}", OPS_12_MEM!());
                CALL!(self.cc.p == PARITY::EVEN);
            }
            0xee => {
                DIS!("XRI D8");
                self.a ^= self.memory.0[(self.pc + 1) as usize];
                self._LogicFlagsA();
                self.pc += 1;
            }
            0xf0 => {
                DIS!("RP {:0>4x}", OPS_12_MEM!());
                RET!(self.cc.s == false);
            }
            0xf1 => {
                DIS!("POP PSW");
                let flags = self.memory.0[self.sp as usize];
                self.a = self.memory.0[(self.sp + 1) as usize];
                self.cc.deserialize(flags);
                self.sp += 2
            }
            0xf2 => {
                DIS!("JP {:0>4x}", OPS_12_MEM!());
                // TODO:: make SIGN::POS SIGN::MIN for cc.s so assembly names are more readable
                J!(self.cc.s == false);
            }
            0xf4 => {
                DIS!("CP {:0>4x}", OPS_12_MEM!());
                CALL!(self.cc.s == false);
            }

            0xf5 => {
                DIS!("PUSH PSW");

                self.memory.0[(self.sp - 1) as usize] = self.a;
                let flags = self.cc.serialize();
                self.memory.0[(self.sp - 2) as usize] = flags;
                self.sp -= 2;
            }
            0xf6 => {
                DIS!("ORI D8");
                self.a |= self.memory.0[(self.pc + 1) as usize];
                self._LogicFlagsA();
                self.pc += 1;
            }
            0xf8 => {
                DIS!("RM {:0>4x}", OPS_12_MEM!());
                RET!(self.cc.s);
            }
            0xf9 => {
                DIS!("SPHL");
                let hl = ((self.h as u16) << 8) | self.l as u16;
                self.sp = hl;
            }
            0xfa => {
                DIS!("JM {:0>4x}", OPS_12_MEM!());
                J!(self.cc.s);
            }
            0xfb => {
                DIS!("EI");
                self.int_enable = true;
            }
            0xfc => {
                DIS!("CM {:0>4x}", OPS_12_MEM!());
                CALL!(self.cc.s);
            }
            0xfe => {
                DIS!("CPI D8");
                let arg1 = self.memory.0[(self.pc + 1) as usize] as u16;
                // subtract and handle underflow
                let x = SUB!(self.a, arg1);
                self._arithFlagsA(x);
                self.pc += 1;
            }

            _ => self._unimplemented_instruction(),
        }
        self.pc += 1;
        self.cycles += State8080::INSTR_CYCLES[opcode as usize] as u64;
    }

    fn _unimplemented_instruction(&self) {
        let opcode: u8 = self.memory.0[self.pc as usize];

        println!(
            "Unimplemented Instruction with opcode 0x{:0>2x} self: {:?}",
            opcode, self
        );
        std::process::exit(1);
    }

    fn _todo() {
        println!("Coming soon");
        std::process::exit(1);
    }
}
fn main() {
    use drawille::Canvas;
    use std::env;
    let mut canvas = Canvas::new(224, 256);

    let args: Vec<String> = env::args().collect();
    let num_instr = args[1].parse().unwrap_or(50000);
    let state: &mut State8080 = &mut Default::default();

    let f = fs::read("./rom/invaders").expect("Unable to read ROM file");
    // read rom into RAM memory
    for (i, c) in f.iter().enumerate() {
        state.memory.0[i] = *c as u8;
    }

    // print!("{}[2J", 27 as char);
    let mut v = Vec::new();
    for i in 0..=num_instr {
        // print!("{}\t", i);
        state.emulate();
        let framebuffer = &state.memory.0[0x2400..=0x3FFF];
        if i < 40000 {
            continue;
        }
        use std::time::Instant;
        let now = Instant::now();
        canvas.clear();
        for (i, p) in framebuffer.iter().enumerate() {
            for ii in (0..8).rev() {
                let ind = i * 8 + 8 - ii;
                if *p & 1 << ii != 0 {
                    canvas.set((ind / 256) as u32, (ind % 256) as u32);
                }
            }
        }
        print!("{}[2J", 27 as char);
        let stdout1 = std::io::stdout();
        let mut stdout = stdout1.lock();
        use std::io::Write;
        // std::io::copy(reader: &mut R, writer: &mut W)
        stdout.write_all(canvas.frame().as_bytes()).unwrap();
        v.push(now.elapsed().as_millis());
        if i > 50000 {
            break;
        }
        // write!(stdout, canvas.frame());
    }

    print!("{}[2J", 27 as char);
    print!("{:?}", v);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parity() {
        use PARITY::*;
        assert_eq!(_parity(0, 0), EVEN);
        assert_eq!(_parity(0, 4), EVEN);
        assert_eq!(_parity(0, 1), EVEN);
        assert_eq!(_parity(1, 1), ODD);
        assert_eq!(_parity(0x07, std::mem::size_of::<u8>()), ODD);
        assert_eq!(_parity(0x06, std::mem::size_of::<u8>()), EVEN);
        assert_eq!(_parity(0x05, std::mem::size_of::<u8>()), EVEN);

        assert_eq!(_parity(2, 2), ODD); // 2 = 0b10
        assert_eq!(_parity(0x00f000, 4), EVEN);
        assert_eq!(_parity(0x00f001, 4), ODD);
        // only bottom byte should matter
        assert_eq!(_parity(0xffffef01, 1), ODD);
        assert_eq!(_parity(0xffefef01, 1), ODD);

        assert_eq!(0, 0);
    }

    #[test]
    fn cpu_diag() -> Result<(), String> {
        let state: &mut State8080 = &mut Default::default();
        let f = fs::read("./rom/cpudiag.bin").expect("Unable to read cpudiag ROM file");
        state.load_rom(f);
        state.init_cpudiag();
        for i in 0..611 {
            print!("{} ", i);
            state.emulate();
        }
        Ok(())
    }
}
