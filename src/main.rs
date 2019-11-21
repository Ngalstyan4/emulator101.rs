extern crate drawille;

use std;
use std::fs;
mod cpu8080;
mod screen;
use cpu8080::State8080;
use screen::Screen;
fn main() {
    use std::env;

    let args: Vec<String> = env::args().collect();
    let num_instr = args[1].parse().unwrap_or(50000);
    let state: &mut State8080 = &mut Default::default();
    let f = fs::read("./rom/invaders").expect("Unable to read ROM file");
    state.load_rom(f);

    // print!("{}[2J", 27 as char);
    let mut v = Vec::new();
    let mut screen: Screen = Screen::new();
    use std::time::Instant;

    let mut last_int = Instant::now();
    for i in 0..=num_instr {
        // print!("{}\t", i);
        let now = Instant::now();

        if last_int.elapsed().as_millis() < now.elapsed().as_millis() + 16 {
            state.interrupt(1);
            state.interrupt(2);
            last_int = now;
        }
        state.emulate();
        let framebuffer = state.get_framebuffer();

        if i < 400000 {
            continue;
        }
        screen.clear();
        screen.update(framebuffer);
        screen.render();
        v.push(now.elapsed().as_millis());
        // if i > 50000 {
        //     break;
        // }
    }

    print!("{}[2J", 27 as char);
    print!("{:?}", v);
}
