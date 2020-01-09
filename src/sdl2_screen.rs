extern crate sdl2;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Point;
use std::time::Duration;

use std::{thread, time};

use crate::screen::Screen;

pub struct Sdl2Screen {
    canvas: sdl2::render::Canvas<sdl2::video::Window>,
    event_handler: sdl2::EventPump,
    background_color: Color,
    foreground_color: Color,
}

impl Sdl2Screen {
    pub fn new_from_ctx(
        mut canvas: sdl2::render::Canvas<sdl2::video::Window>,
        event_pump: sdl2::EventPump,
    ) -> Self {
        let background_color = Color::RGB(0, 0, 0);
        canvas.set_draw_color(background_color);
        canvas.set_scale(3.0, 3.0).unwrap();
        canvas.clear();
        canvas.present();
        Sdl2Screen {
            canvas: canvas,
            event_handler: event_pump,
            background_color: background_color,
            foreground_color: Color::RGB(255, 255, 255),
        }
    }
}

impl Sdl2Screen {
    fn draw_elems<'a>(&mut self, elems: Vec<(usize, &'a u8)>, c:Color) {
        for (i, p) in elems {
            for ii in (0..8).rev() {
                let ind = i * 8 + 8 - ii;
                if *p & 1 << ii != 0 {
                    self.canvas
                        .draw_point(Point::new((ind / 256) as i32, (ind % 256) as i32))
                        .unwrap();
                }
            }
        }
    }
}

impl Screen for Sdl2Screen {
    //todo Q:: this is broken as the context gets dropped at the end of the call while we need it for canvas
    // Is this a bug of the package or something I will have to account for in another way?
    /********************************  BROKEN CODE BEGIN  *********************************************************/
    fn new() -> Self {
        let sdl_context: sdl2::Sdl = sdl2::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();

        let window = video_subsystem
            .window("rust-sdl2 demo", 400, 600)
            .position_centered()
            .build()
            .unwrap();

        let event_pump: sdl2::EventPump = sdl_context.event_pump().unwrap();

        let mut canvas = window.into_canvas().build().unwrap();
        let background_color = Color::RGB(0, 0, 0);
        canvas.set_draw_color(background_color);
        canvas.clear();
        canvas.present();
        Sdl2Screen {
            canvas: canvas,
            event_handler: event_pump,
            // sdl_ctx:& sdl_context,
            background_color: background_color,
            foreground_color: Color::RGB(255, 255, 255),
        }
    }
    /********************************  BROKEN CODE END  *********************************************************/

    fn clear(&mut self) {
        self.canvas.set_draw_color(self.background_color);
        self.canvas.clear();
    }

    fn update(&mut self, framebuffer: &[u8], top: bool) {
        self.canvas.set_draw_color(self.foreground_color);
        // Q:: Can I avoid collecting the vector? and pass an iterator instead?
        let elems = framebuffer.iter().enumerate().filter(|&(i, &v)| (i *8 % 256 < 112 )== top).collect::<Vec<_>>();
        // let ala: &std::iter::Iterator<Item=&u8> = &[1,2,3,4].iter();
        // ala.enumerate();
        // let elems = (*ala).enumerate();
        self.draw_elems(elems, self.background_color);
    }
    // todo:: Q:: why does sdl require mutable reference for rendering??
    fn render(&mut self) -> u8 {
        let mut ret: u8 = 0;

        for event in self.event_handler.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => {
                    std::process::exit(0);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Left),
                    ..
                } => {
                    ret = 0x20;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Right),
                    ..
                } => {
                    ret = 0x40;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::C),
                    ..
                } => {
                    ret = 0x1;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Space),
                    ..
                } => {
                    ret = 0x10;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::S),
                    ..
                } => {
                    ret = 0x04;
                }
                // Event::KeyUp { .. } => {
                //     ret = 0;
                // }

                _ => {}
            }
        }
        self.canvas.present();
        return ret;

        // let ten_millis = time::Duration::from_millis(10);
        // let now = time::Instant::now();
        // std::thread::sleep(Duration::new(0, 1_00u32 / 60))

        // thread::sleep(ten_millis);
    }
}

// canvas.set_draw_color(Color::RGB(0, 255, 255));
// canvas.clear();
// canvas.present();
// let mut event_pump = sdl_context.event_pump().unwrap();
// let mut i = 0;
// 'running: loop {
//     canvas.set_draw_color(Color::RGB(0, 255, 255));
//     canvas.clear();
//     // i = (i + 1) % 255;
//     canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
//     for j in 0..400{
//         for k in 0..100 {
//             canvas.set_draw_color(Color::RGB(0, 0, 0));
//             canvas.draw_point(Point::new(j, k));

//         }
//     }

//     // canvas.clear();
//     for event in event_pump.poll_iter() {
//         match event {
//             Event::Quit {..} |
//             Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
//                 break 'running
//             },
//             _ => {}
//         }
//     }
//     // The rest of the game loop goes here...

//     canvas.present();
//     ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
