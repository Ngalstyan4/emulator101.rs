use std;
use std::fs;


#[derive(PartialEq)]
#[derive(Debug)]
enum PARITY {EVEN, ODD}

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
    z:bool,
    s:bool,
    p:PARITY,
    cy:bool,
    ac:u8,// noone uses
    pad:u8
}

impl std::fmt::Debug for CC {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let z = if self.z {"z"} else {"."};
        let s = if self.s {"s"} else {"."};
        let p = if self.p == PARITY::ODD {"p"} else {"."};
        let cy = if self.cy {"y"} else {"."};
        let pad = if self.pad != 0 {"^"} else {"."};
        write!(f, "{}{}{}{}{}",z,s,p,cy,pad)
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
struct RAM(Box<[u8;2*0x10_000]>); // C version uses u16 but this is easier played in rust

impl std::fmt::Debug for RAM {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "RAM")
    }
}

impl Default for RAM{
    fn default() -> Self {
        RAM(Box::new([0;2*0x10_000]))// Q:: can I do this without box?
    }
}
#[derive(Default)]
struct State8080 {
    a:u8,
    b:u8,
    c:u8,
    d:u8,
    e:u8,
    h:u8,
    l:u8,
    sp:u16,
    pc:u16,
    memory:RAM,
    cc:CC,
    int_enable:u8,
}

impl std::fmt::Debug for State8080 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "a: {:0>4x}, bc: {:0>2x}{:0>2x}, de: {:0>2x}{:0>2x} hl:{:0>2x}{:0>2x} pc:{:0>4x} sp:{:0>4x}  flags:{:?}",self.a,   self.b,self.c,  self.d, self.e,
        self.h, self.l,     self.pc, self.sp, self.cc)
    }
}

/* HELPER UTILITY FUNCTIONS BEGIN */
        // Q:: can I make this type generic?
        fn _parity(num: u32, size: usize) -> PARITY{ // TODO:: parity broken look at instruction number 1522-1554
            let num_bits = size * 8;
            let s = format!("{:0>32b}", num);//.chars();//.skip(32 - num_bits);
            // HELP Q:: THis must be super super slow and inefficient.
            s.chars().skip(32-num_bits).fold(PARITY::EVEN, |acc,c| {if c == '1' {!acc} else {acc}})
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
    const DISASSEMBLE:bool = true;

    pub fn emulate(&mut self){

        /* INSTRUCTION MACROS BEGIN */
        macro_rules! DIS {
                ($fmt:expr) => (if State8080::DISASSEMBLE {print!($fmt); println!("\t\t\t {:?}", self)});
                ($fmt:expr, $($arg:tt)*) => (if State8080::DISASSEMBLE { print!($fmt, $($arg)*); println!("\t\t\t {:?}", self)});
        }

        macro_rules! DCR {
            ($reg:ident) => {
                let res = if self.$reg == 0 {std::u8::MAX} else {self.$reg-1};
                self.cc.z = res == 0;
                self.cc.s = 0x80 == (res & 0x80);
                self.cc.p = _parity(res as u32, std::mem::size_of::<u8>());
                self.$reg = res;
            };
        }

        macro_rules! DAD {
            ($reglo:ident, $reghi:ident) => {
                let hl:u32 = ((self.h as u32) << 8 )| (self.l as u32);

                let xy:u32 = ((self.$reglo as u32) << 8) | self.$reghi as u32;
                let res = hl + xy;
                self.h = ((res & 0xff00) >> 8) as u8;
			    self.l = (res & 0xff) as u8;
			    self.cc.cy = (res & 0xffff0000) > 0;
            };
        }

        macro_rules! MOVRM {
            ($reg:ident) => {
                let offset = (self.h as u32) << 8 | self.l as u32;
                self.$reg = self.memory.0[offset as usize];
                self.pc += 1;
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
                    }
                    else {
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
                self.$fst = self.memory.0[(self.sp - 1) as usize];
                self.$snd = self.memory.0[(self.sp - 2) as usize];
                self.sp += 2;
            };
        }

        /* INSTRUCTION MACROS END   */


        let opcode:u8 = self.memory.0[self.pc as usize];
        if State8080::DISASSEMBLE {
            print!("0x{:0>2x}\t", opcode);
        }
        match opcode {
            0x00 => DIS!("NOOP"),
            0x01 => {
                DIS!("LXI B, #${:02x}{:02x}", self.memory.0[self.pc as usize +2], self.memory.0[self.pc as usize +1]);
                self.c = self.memory.0[self.pc as usize +1];
                self.b = self.memory.0[self.pc as usize +2];
                self.pc += 2;
            }
            0x02..=0x04 => self._unimplemented_instruction(),
            0x05 => {
                DIS!("DCR B");DCR!(b);
            }
// 0x06	MVI B,D8
            0x06=> { DIS!("MVI B,D8");MOVRM!(b);}
            0x07..=0x08 => self._unimplemented_instruction(),
            0x09        => {
                DIS!("DAD ,B");DAD!(b,c);
            }
            0x0a..=0x0c => self._unimplemented_instruction(),
            0x0d => {
                DIS!("DCR C");DCR!(c);
            },
            0x0e => {
                DIS!("MVI C,D8");
                self.c = self.memory.0[(self.pc + 1) as usize];
                self.pc += 1;
            },
            0x0f => {
                DIS!("RRC");
                let x = self.a;
				self.a = ((x & 1) << 7) | (x >> 1);
				self.cc.cy = 1 == (x&1);
            },
            0x10 => self._unimplemented_instruction(),
            0x11 => {
                DIS!("LXI D,D16");
                self.e = self.memory.0[(self.pc + 1) as usize];
			    self.d = self.memory.0[(self.pc + 2) as usize];
                self.pc += 2;
            }
            0x13 => {DIS!("INX D"); INC!(d,e);},
            0x19 => {DIS!("DAD D"); DAD!(d,e);},
            0x1a => {DIS!("LDAX D");
                let offset = (self.d as u32) << 8 | self.e as u32;
                self.a = self.memory.0[offset as usize];
        },
            0x21 => {DIS!("LXI H,D16");
                self.l = self.memory.0[(self.pc + 1) as usize];
                self.h = self.memory.0[(self.pc + 2) as usize];
                self.pc += 2;
        },
            0x23 => {DIS!("INX H");/* INX HL */ INC!(h,l);
        },
            0x26 => {DIS!("MVI H,D8");
                let arg1 = self.memory.0[(self.pc + 1) as usize];
                self.h = arg1;
                self.pc += 1;
        
        
        },
            0x29 => {DIS!("DAD H"); DAD!(h,l);},
            0x31 => {DIS!("LXI SP,D16");
            self.sp = (self.memory.0[(self.pc + 2) as usize] as u16) | self.memory.0[(self.pc + 1) as usize] as u16 ;
			self.pc += 2;
        },
            0x32 => {DIS!("STA adr"); State8080::_todo()},
            0x36 => {DIS!("MVI M,D8");
                let offset = (self.h as u32) << 8 | self.l as u32;
                self.memory.0[offset as usize] = self.memory.0[(self.pc + 1) as usize];
                self.pc += 1;
            },
            0x3a => {DIS!("LDA adr"); State8080::_todo()},
            0x3e => {DIS!("MVI A,D8"); State8080::_todo()},

            0x56 => {DIS!("MOV D,M");MOVRM!(d);},
            0x5e => {DIS!("MOV E,M");MOVRM!(e);},
            0x66 => {DIS!("MOV H,M");MOVRM!(d);},

            0x6f => {DIS!("MOV L,A");MOVRR!(l,a);},

            0x77 => {DIS!("MOV M,A"); MOVMR!(a);},

            0x7a => {DIS!("MOV A,D"); MOVRR!(a,d);},
            0x7b => {DIS!("MOV A,E"); MOVRR!(a,e);},
            0x7c => {DIS!("MOV A,H"); MOVRR!(a,h);},

            0x7e => {DIS!("MOV A,M"); MOVRM!(a);},

            0xa7 => {DIS!("ANA A"); State8080::_todo()},
            0xaf => {DIS!("XRA A"); State8080::_todo()},
            0xc1 => {DIS!("POP B"); POP!(c,b);},
            0xc2 => {
                let adr = (self.memory.0[(self.pc + 2) as usize] as u32) << 8 | (self.memory.0[(self.pc + 1) as usize] as u32);
                DIS!("JNZ {:0>4x}", adr);
                /* Q:: should this not be != i.e. jump IF NOT ZERO <-- JNZ */
                if self.cc.z == false {
                    self.pc = adr as u16;
                    return;
                } else {
                    self.pc += 2;
                }
        },
            0xc3 => {
                let adr = (self.memory.0[(self.pc + 2) as usize] as u32) << 8 | (self.memory.0[(self.pc + 1) as usize] as u32);
                DIS!("JMP {:0>4x}", adr);
                /*self.pc += 2*/
                self.pc = adr as u16;
                return;
                },
            0xc5 => {DIS!("PUSH B"); PUSH!(b,c);},
            0xc6 => {DIS!("ADI D8"); State8080::_todo()},
            0xc9 => {DIS!("RET");
                    self.pc = ((self.memory.0[(self.sp+1) as usize] as u32) << 8 | self.memory.0[self.sp as usize] as u32) as u16;
                    self.sp += 2;
                    return;
        },
            0xcd => {
                let adr = (self.memory.0[(self.pc + 2) as usize] as u32) << 8 | (self.memory.0[(self.pc + 1) as usize] as u32);
                DIS!("CALL {:0>4x}", adr);
                let	ret = self.pc+3; //todo:: may be a bug here q::
                self.memory.0[(self.sp-1) as usize] = ((ret >> 8) & 0xff) as u8;
                self.memory.0[(self.sp-2) as usize] = (ret & 0xff) as u8;
                self.sp -= 2;
                self.pc = adr as u16;
                return;
                },
            0xd1 => {DIS!("POP D"); State8080::_todo()},
            0xd3 => {DIS!("OUT D8"); self.pc +=1; /* not really implemented yet todo::*/ },
            0xd5 => {DIS!("PUSH D"); PUSH!(d,e); },
            0xe1 => {DIS!("POP H"); POP!(l,h);},
            0xe5 => {DIS!("PUSH H"); PUSH!{h,l}},
            0xe6 => {DIS!("ANI D8"); State8080::_todo()},
            0xeb => {DIS!("XCHG");
            let save1 = self.d;
			let save2 = self.e;
				self.d = self.h;
				self.e = self.l;
				self.h = save1;
				self.l = save2;
        },
            0xf1 => {DIS!("POP PSW"); State8080::_todo()},

            0xf5 => {DIS!("PUSH PSW"); State8080::_todo()},
            0xfb => {DIS!("EI"); State8080::_todo()},
            0xfe => {DIS!("CPI D8");
                let arg1 = self.memory.0[(self.pc + 1) as usize];
                // todo:: Q:: make it signed to make sure subtraction does not panic
                // is this how it should be done?
                let x = (self.a as i32 - arg1 as i32) as u32;
			    self.cc.z = x == 0;
			    self.cc.s = 0x80 == (x & 0x80);
			    self.cc.p = _parity(x, std::mem::size_of::<u8>());
    			self.cc.cy = self.a < arg1;
	    		self.pc += 1;
        },

            _ => self._unimplemented_instruction(),
        }
        self.pc+=1;

    }

    fn _unimplemented_instruction(&self) {
        let opcode:u8 = self.memory.0[self.pc as usize];
        println!("Unimplemented Instruction with opcode 0x{:0>2x}", opcode);
        std::process::exit(1);
    }

    fn _todo() {
        println!("Coming soon");
        std::process::exit(1);

    }

}
fn main() {

    let state:&mut State8080 = &mut Default::default();
    let f = fs::read("./rom/invaders").expect("Unable to read ROM file");
    // f.read(&mut state.memory.0).unwrap();
    state.memory.0[0]  = 0xff;
    // read rom into RAM memory
    for (i,c) in f.iter().enumerate(){
        state.memory.0[i] = *c as u8;
    }
    print!("{}[2J", 27 as char);
    for i in 0.. {
        print!("{}\t",i);
        state.emulate();
    }
    
}

#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_parity(){
        use PARITY::*;
        assert_eq!(_parity(0, 0), EVEN);
        assert_eq!(_parity(0, 4), EVEN);
        assert_eq!(_parity(2, 2), ODD);// 2 = 0b10
        assert_eq!(_parity(0x00f000, 4), EVEN);
        assert_eq!(_parity(0x00f001, 4), ODD);
        // only bottom byte should matter
        assert_eq!(_parity(0xffffef01, 1), ODD);
        assert_eq!(_parity(0xffefef01, 1), ODD);


        assert_eq!(0,0);
    }
}

