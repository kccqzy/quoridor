use std::io::Write;

fn draw_unicode_box(width: u8, height: u8) -> String {
    assert!(height > 0);
    assert!(width > 0);
    let mut out = String::new();
    for r in 0..height {
        // Draw the top-border.
        for c in 0..width {
            out.push(if r == 0 && c == 0 {
                '┌'
            } else if r == 0 {
                '┬'
            } else if c == 0 {
                '├'
            } else {
                '┼'
            });
            out.push_str("───");
        }
        // Draw right border
        out.push(if r == 0 { '┐' } else { '┤' });
        out.push('\n');
        // Draw the middle
        for _c in 0..width {
            out.push_str("│   ");
        }
        out.push_str("│\n");
    }
    // Draw the bottom
    for c in 0..width {
        out.push(if c == 0 { '└' } else { '┴' });
        out.push_str("───");
    }
    out.push_str("┘\n");
    out
}

fn main() {
    println!("Hello, world!");
    std::io::stdout().lock().write_all(draw_unicode_box(9, 9).as_bytes()).unwrap();
}
