pub trait Screen {
    fn new() -> Self; //  Q:: is that the right thing to do? // should this be out of the trait?
    fn clear(&mut self);
    fn update(&mut self, framebuffer: &[u8]);
    fn render(&mut self);
}
