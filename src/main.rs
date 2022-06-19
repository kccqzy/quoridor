use std::io::Write;

#[derive(PartialEq, Eq)]
enum FenceOrientation {
    Horizontal,
    Vertical,
}

fn draw_unicode_box(
    width: u8, height: u8, canonical_fence: impl Fn(u8, u8) -> Option<FenceOrientation>,
) -> String {
    let vertical_fence_left = |r: u8, c: u8| {
        // A vertical fence left of (r,c) exists iff canonical fence (r,c-1) or (r-1,c-1)
        c > 0
            && (canonical_fence(r, c - 1) == Some(FenceOrientation::Vertical)
                || r > 0 && canonical_fence(r - 1, c - 1) == Some(FenceOrientation::Vertical))
    };
    let horizontal_fence_top = |r: u8, c: u8| {
        r > 0
            && (canonical_fence(r - 1, c) == Some(FenceOrientation::Horizontal)
                || c > 0 && canonical_fence(r - 1, c - 1) == Some(FenceOrientation::Horizontal))
    };

    assert!(height > 0);
    assert!(width > 0);
    let mut out = String::new();
    for r in 0..height {
        // Draw the top-border.
        for c in 0..width {
            out.push(if r == 0 && c == 0 {
                '┌'
            } else if r == 0 {
                if vertical_fence_left(r, c) {
                    '┰'
                } else {
                    '┬'
                }
            } else if c == 0 {
                if horizontal_fence_top(r, c) {
                    '┝'
                } else {
                    '├'
                }
            } else {
                match (
                    vertical_fence_left(r - 1, c),
                    vertical_fence_left(r, c),
                    horizontal_fence_top(r, c - 1),
                    horizontal_fence_top(r, c),
                ) {
                    (false, false, false, false) => '┼',
                    (false, false, true, false) => '┽',
                    (false, false, false, true) => '┾',
                    (false, false, true, true) => '┿',
                    (true, false, false, false) => '╀',
                    (false, true, false, false) => '╁',
                    (true, true, false, false) => '╂',
                    (true, false, true, false) => '╃',
                    (true, false, false, true) => '╄',
                    (false, true, true, false) => '╅',
                    (false, true, false, true) => '╆',
                    (true, false, true, true) => '╇',
                    (false, true, true, true) => '╈',
                    (true, true, true, false) => '╉',
                    (true, true, false, true) => '╊',
                    (true, true, true, true) => '╋',
                }
            });
            out.push_str(if horizontal_fence_top(r, c) { "━━━" } else { "───" });
        }
        // Draw right border
        out.push(if r == 0 {
            '┐'
        } else if horizontal_fence_top(r, width - 1) {
            '┥'
        } else {
            '┤'
        });
        out.push('\n');
        // Draw the middle
        for c in 0..width {
            out.push(if vertical_fence_left(r, c) { '┃' } else { '│' });
            out.push_str("   ");
        }
        out.push_str("│\n");
    }
    // Draw the bottom
    for c in 0..width {
        out.push(if c == 0 {
            '└'
        } else if vertical_fence_left(height - 1, c) {
            '┸'
        } else {
            '┴'
        });
        out.push_str("───");
    }
    out.push_str("┘\n");
    out
}

fn main() {
    println!("Hello, world!");
    std::io::stdout()
        .lock()
        .write_all(
            draw_unicode_box(9, 9, |r, c| {
                if r == 0 && c == 0 {
                    Some(FenceOrientation::Horizontal)
                } else if r == 7 && c == 7 {
                    Some(FenceOrientation::Vertical)
                } else {
                    None
                }
            })
            .as_bytes(),
        )
        .unwrap();
}
