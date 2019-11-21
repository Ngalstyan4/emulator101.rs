use drawille::Canvas;
// let mut canvas = Canvas::new(224, 256);

pub struct Screen {
    canvas: Canvas,
}

impl Screen {
    pub fn new() -> Screen {
        Screen {
            canvas: Canvas::new(224, 256),
        }
    }

    pub fn clear(&mut self) {
        self.canvas.clear();
    }

    pub fn update(&mut self, framebuffer: &[u8]) {
        for (i, p) in framebuffer.iter().enumerate() {
            for ii in (0..8).rev() {
                let ind = i * 8 + 8 - ii;
                if *p & 1 << ii != 0 {
                    self.canvas.set((ind / 256) as u32, (ind % 256) as u32);
                }
            }
        }
    }

    pub fn render(&mut self) {
        print!("{}[2J", 27 as char);
        let stdout1 = std::io::stdout();
        let mut stdout = stdout1.lock();
        use std::io::Write;
        stdout.write_all(self.canvas.frame().as_bytes());

        // std::io::copy(reader: &mut R, writer: &mut W)
    }
}
