use crate::cpu8080::State8080;
use crate::screen::Screen;
use crate::sdl2_screen::Sdl2Screen;

struct SoC<S: Screen> {
    cpu: State8080,
    screen: S,
    port: u8,
    shift0: u8,
    shift1: u8,
    shift_offset: u8,
}

impl SoC<Sdl2Screen> {
    fn emulate(self) {
        self.cpu;
    }
}
