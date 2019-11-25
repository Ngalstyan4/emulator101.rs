use crate::screen::Screen;
use crate::cpu8080::State8080;

struct SoC<T: Screen> {
    cpu: State8080,
    screen: T,
    port: u8,
    shift0:u8,
    shift1:u8,
    shift_offset:u8,
}

