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
            PARITY::default()
        } else {
            !PARITY::default()
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
    const DISASSEMBLE: bool = true;

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
        self.cc.ac = 0;
        self.cc.z = self.a == 0;
        self.cc.s = 0x80 == (self.a & 0x80);
        self.cc.p = _parity(self.a as u32, std::mem::size_of::<u8>())
    }

    fn _arithFlagsA(&mut self, res: u16) {
        self.cc.cy = res > 0xff;
        self.cc.z = (res & 0xff) == 0;
        self.cc.s = 0x80 == (res & 0x80);
        self.cc.p = _parity((res & 0xff) as u32, std::mem::size_of::<u8>());
    }

    pub fn emulate(&mut self) {
        /* INSTRUCTION MACROS BEGIN */
        macro_rules! DEREF_HL {
            () => {
                self.memory.0[(((self.h as u32) << 8) | (self.l as u32)) as usize]
            };
        }
        macro_rules! DIS {
                ($fmt:expr) => (if State8080::DISASSEMBLE {print!(concat!($fmt,"{0: >21}")," "); println!("\t{:?}", self)});
                ($fmt:expr, $($arg:tt)*) => (if State8080::DISASSEMBLE { print!($fmt, $($arg)*); println!("\t\t\t{:?}", self)});
        }

        macro_rules! DCR {
            ($reg:ident) => {
                let res = if self.$reg == 0 {
                    std::u8::MAX
                } else {
                    self.$reg - 1
                };
                self.cc.z = res == 0;
                self.cc.s = 0x80 == (res & 0x80);
                self.cc.p = _parity(res as u32, std::mem::size_of::<u8>());
                self.$reg = res;
            };
        }

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

        macro_rules! XOR {
            ($reg:ident) => {
                self.$reg = self.$reg ^ self.$reg;
                self._LogicFlagsA();
            };
        }

        macro_rules! AND {
            ($reg:ident) => {
                self.$reg = self.$reg & self.$reg;
                self._LogicFlagsA();
            };
        }

        macro_rules! CMP {
            ($reg:ident) => {
                // log. or makes sure the result is never negative
                // let res = (self.a as u16 | 0x0001) - self.$reg as u16;
                let res = self.a as u16 - self.$reg as u16; // todo:: this may panic
                self._arithFlagsA(res);
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
            0x02..=0x04 => self._unimplemented_instruction(),
            0x05 => {
                DIS!("DCR B");
                DCR!(b);
            }
            // 0x06	MVI B,D8
            0x06 => {
                let offset = self.pc + 1;
                let arg = self.memory.0[offset as usize];
                DIS!("MVI B,$#{:0>4x}", arg);
                MOVRI!(b);
            }
            0x07..=0x08 => self._unimplemented_instruction(),
            0x09 => {
                DIS!("DAD ,B");
                DAD!(b, c);
            }
            0x0a..=0x0c => self._unimplemented_instruction(),
            0x0d => {
                DIS!("DCR C");
                DCR!(c);
            }
            0x0e => {
                DIS!("MVI C,D8");
                self.c = self.memory.0[(self.pc + 1) as usize];
                self.pc += 1;
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
            0x13 => {
                DIS!("INX D");
                INC!(d, e);
            }
            0x19 => {
                DIS!("DAD D");
                DAD!(d, e);
            }
            0x1a => {
                DIS!("LDAX D");
                let offset = (self.d as u32) << 8 | self.e as u32;
                self.a = self.memory.0[offset as usize];
            }
            0x21 => {
                DIS!("LXI H,D16");
                self.l = self.memory.0[(self.pc + 1) as usize];
                self.h = self.memory.0[(self.pc + 2) as usize];
                self.pc += 2;
            }
            0x23 => {
                DIS!("INX H"); /* INX HL */
                INC!(h, l);
            }
            0x26 => {
                DIS!("MVI H,D8");
                let arg1 = self.memory.0[(self.pc + 1) as usize];
                self.h = arg1;
                self.pc += 1;
            }
            0x29 => {
                DIS!("DAD H");
                DAD!(h, l);
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
            0x36 => {
                DIS!("MVI M,D8");
                let offset = (self.h as u32) << 8 | self.l as u32;
                self.memory.0[offset as usize] = self.memory.0[(self.pc + 1) as usize];
                self.pc += 1;
            }
            0x3a => {
                DIS!("LDA adr");
                let offset = ((self.memory.0[(self.pc + 2) as usize] as u32) << 8)
                    | (self.memory.0[(self.pc + 1) as usize] as u32);
                self.a = self.memory.0[offset as usize];
                self.pc += 2;
            }
            0x3e => {
                DIS!("MVI A,D8");
                MOVRI!(a);
            }

            0x56 => {
                DIS!("MOV D,M");
                MOVRM!(d);
            }
            0x5e => {
                DIS!("MOV E,M");
                MOVRM!(e);
            }
            0x66 => {
                DIS!("MOV H,M");
                MOVRM!(h);
            }

            0x6f => {
                DIS!("MOV L,A");
                MOVRR!(l, a);
            }

            0x77 => {
                DIS!("MOV M,A");
                MOVMR!(a);
            }

            0x7a => {
                DIS!("MOV A,D");
                MOVRR!(a, d);
            }
            0x7b => {
                DIS!("MOV A,E");
                MOVRR!(a, e);
            }
            0x7c => {
                DIS!("MOV A,H");
                MOVRR!(a, h);
            },
            0x7d => {
                DIS!("MOV A,L");
                MOVRR!(a, l);
            }

            0x7e => {
                DIS!("MOV A,M");
                MOVRM!(a);
            }

            0xa7 => {
                DIS!("AND A");
                AND!(a);
            }
            0xaf => {
                DIS!("XOR A");
                XOR!(a);
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
                let mem_hl = DEREF_HL!();
                let res = self.a as u16 - mem_hl as u16;// todo: this may panic, see CMP!() as well
                self._arithFlagsA(res);
            }
            0xbf => {
                DIS!("CMP A");
                CMP!(a);
            }

            0xc1 => {
                DIS!("POP B");
                POP!(c, b);
            }

            0xc2 => {
                let adr = (self.memory.0[(self.pc + 2) as usize] as u32) << 8
                    | (self.memory.0[(self.pc + 1) as usize] as u32);
                DIS!("JNZ {:0>4x}", adr);
                /* Q:: should this not be != i.e. jump IF NOT ZERO <-- JNZ */
                if self.cc.z == false {
                    self.pc = adr as u16;
                    return;
                } else {
                    self.pc += 2;
                }
            }
            0xc3 => {
                let adr = (self.memory.0[(self.pc + 2) as usize] as u32) << 8
                    | (self.memory.0[(self.pc + 1) as usize] as u32);
                DIS!("JMP {:0>4x}", adr);
                /*self.pc += 2*/
                self.pc = adr as u16;
                return;
            }
            0xc5 => {
                DIS!("PUSH B");
                PUSH!(b, c);
            }
            0xc6 => {
                DIS!("ADI D8");
                let x = self.a as u16 + self.memory.0[(self.pc + 1) as usize] as u16;
                self.cc.z = (x & 0xff) == 0;
                self.cc.s = 0x80 == (x & 0x80);
                self.cc.p = _parity((x & 0xff) as u32, std::mem::size_of::<u8>());
                self.cc.cy = x > 0xff;
                self.a = x as u8;
                self.pc += 1;
            }
            0xc9 => {
                DIS!("RET");
                self.pc = ((self.memory.0[(self.sp + 1) as usize] as u32) << 8
                    | self.memory.0[self.sp as usize] as u32) as u16;
                self.sp += 2;
                return;
            }
            0xca => {
                DIS!("JZ adr");
                if self.cc.z {
                    self.pc = (((self.memory.0[(self.pc + 2) as usize] as u32) << 8)
                        | self.memory.0[(self.pc + 1) as usize] as u32) as u16;
                return;
                } else {
                    self.pc += 2;
                }
            }
            0xcd => {
                let adr = (self.memory.0[(self.pc + 2) as usize] as u32) << 8
                    | (self.memory.0[(self.pc + 1) as usize] as u32);
                DIS!("CALL {:0>4x}", adr);

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
                        std::process::exit(1);// todo handle the below if this iscommented out
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
            0xd1 => {
                DIS!("POP D");
                POP!(e, d);
            }
            0xd3 => {
                DIS!("OUT D8");
                self.pc += 1; /* not really implemented yet todo::*/
            }
            0xd5 => {
                DIS!("PUSH D");
                PUSH!(d, e);
            }
            0xe1 => {
                DIS!("POP H");
                POP!(l, h);
            },
            0xe3 => {// XTHL	1		L <-> (SP); H <-> (SP+1) 
                DIS!("XTHL");
                let h = self.h;
                let l = self.l;
                self.l = self.memory.0[self.sp as usize];
                self.memory.0[self.sp as usize] = l;
                self.h = self.memory.0[(self.sp+1) as usize];
                self.memory.0[self.sp as usize] = h;
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
            0xeb => {
                DIS!("XCHG");
                let save1 = self.d;
                let save2 = self.e;
                self.d = self.h;
                self.e = self.l;
                self.h = save1;
                self.l = save2;
            }
            0xf1 => {
                DIS!("POP PSW");
                let flags = self.memory.0[self.sp as usize];
                self.a = self.memory.0[(self.sp + 1) as usize];
                self.cc.deserialize(flags);
                self.sp += 2
            }

            0xf5 => {
                DIS!("PUSH PSW");

                self.memory.0[(self.sp - 1) as usize] = self.a;
                let flags = self.cc.serialize();
                self.memory.0[(self.sp - 2) as usize] = flags;
                self.sp -= 2;
            }
            0xfb => {
                DIS!("EI");
                self.int_enable = true;
            }
            0xfe => {
                DIS!("CPI D8");
                let arg1 = self.memory.0[(self.pc + 1) as usize];
                // todo:: Q:: make it signed to make sure subtraction does not panic
                // is this how it should be done?
                let x = (self.a as i32 - arg1 as i32) as u32;
                self.cc.z = x == 0;
                self.cc.s = 0x80 == (x & 0x80);
                self.cc.p = _parity(x, std::mem::size_of::<u8>());
                self.cc.cy = self.a < arg1;
                self.pc += 1;
            }

            _ => self._unimplemented_instruction(),
        }
        self.pc += 1;
        self.cycles += State8080::INSTR_CYCLES[opcode as usize] as u64;
    }

    fn _unimplemented_instruction(&self) {
        let opcode: u8 = self.memory.0[self.pc as usize];
        
        println!("Unimplemented Instruction with opcode 0x{:0>2x} self: {:?}", opcode, self);
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
        use std::time::{Duration, Instant};
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
        stdout.write_all(canvas.frame().as_bytes());
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
    fn cpu_diag() {
        let state: &mut State8080 = &mut Default::default();
        let f = fs::read("./rom/cpudiag.bin").expect("Unable to read cpudiag ROM file");
        state.load_rom(f);
        state.init_cpudiag();
        for _ in 0..1000 {
            state.emulate();
        }
    }
}
