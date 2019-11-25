#![feature(try_from)]

use std;
use std::fs;
mod cpu8080;
mod screen;
mod sdl2_screen;
// mod terminal;

mod soc;
use cpu8080::State8080;

use crate::screen::Screen;
use sdl2_screen::Sdl2Screen;
// use terminal::Terminal;

fn main() {
    use std::env;

    let args: Vec<String> = env::args().collect();
    let num_instr = args[1].parse().unwrap_or(50000);
    let state: &mut State8080 = &mut Default::default();
    let f = fs::read("./rom/invaders").expect("Unable to read ROM file");
    state.load_rom(f);

    let sdl_context: sdl2::Sdl = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("Space Invaders!", 3 * 224, 3 * 256)
        .position_centered()
        .build()
        .unwrap();

    let event_pump: sdl2::EventPump = sdl_context.event_pump().unwrap();

    let canvas = window.into_canvas().build().unwrap();

    let mut screen = Sdl2Screen::new_from_ctx(canvas, event_pump);
    screen.clear();
    screen.render();
    use std::time::Instant;

    let mut last_int = Instant::now();
    use std::time::Duration;
    // 'running: loop {
    //     screen.render();
    //     std::thread::sleep(Duration::new(0, 1_000_000_000u32/6));
    // }
    let mut int = 1;
    for i in 0..=num_instr {
        // print!("{}\t", i);
        let now = Instant::now();
        if last_int.elapsed().as_millis() < now.elapsed().as_millis() + 8000 {
            state.interrupt(int);
            int = 1+ int % 2;
            // state.interrupt(2);
            last_int = now;
        }
        state.emulate();
        if i < 50000 {
            continue;
        };
        let framebuffer = state.get_framebuffer();
        screen.clear();
        screen.update(framebuffer);
        state.set_port(screen.render());
    }
}

// extern crate sdl2;

// use sdl2::pixels::Color;
// use sdl2::event::Event;
// use sdl2::keyboard::Keycode;
// use std::time::Duration;

// pub fn main() {
//     let sdl_context = sdl2::init().unwrap();
//     let video_subsystem = sdl_context.video().unwrap();
//     let window = video_subsystem.window("rust-sdl2 demo", 800, 600)
//         .position_centered()
//         .build()
//         .unwrap();
//     let mut canvas = window.into_canvas().build().unwrap();
//     let mut event_pump = sdl_context.event_pump().unwrap();
//     window.drop
//     canvas.present();

//     for event in event_pump.poll_iter() {
//         match event {
//             Event::Quit {..} |
//             Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
//             std::process::exit(0);
//             },
//             _ => {}
//         }
//     }
//     // canvas.clear();
//     ::std::thread::sleep(Duration::new(0, 4_000_000_000u32));
//     }
