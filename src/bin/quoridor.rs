use quoridor::draw_unicode_box;
use quoridor::FenceOrientation;
use std::io::Write;

fn main() {
    let vertical_label = ['9', '8', '7', '6', '5', '4', '3', '2', '1'];
    let horizontal_label = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'];
    std::io::stdout()
        .lock()
        .write_all(
            draw_unicode_box(
                &horizontal_label,
                &vertical_label,
                |r, c| {
                    if r == 0 && c == 4 {
                        '■'
                    } else if r == 8 && c == 4 {
                        '○'
                    } else {
                        ' '
                    }
                },
                |r, c| {
                    if r == 0 && c == 0 {
                        Some(FenceOrientation::Horizontal)
                    } else if r == 7 && c == 7 {
                        Some(FenceOrientation::Vertical)
                    } else {
                        None
                    }
                },
            )
            .as_bytes(),
        )
        .unwrap();
}
