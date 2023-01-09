pub trait Support {
    fn print(&mut self, channel: usize, message: &str);
    fn print_error(&mut self, message: &str);
}
