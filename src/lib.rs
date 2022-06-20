#[derive(PartialEq, Eq)]
pub enum FenceOrientation {
    Horizontal,
    Vertical,
}

pub fn draw_unicode_box(
    horizontal_label: &[char], vertical_label: &[char],
    in_box_label: impl Fn(usize, usize) -> char,
    canonical_fence: impl Fn(usize, usize) -> Option<FenceOrientation>,
) -> String {
    let vertical_fence_left = |r: usize, c: usize| {
        // A vertical fence left of (r,c) exists iff canonical fence (r,c-1) or (r-1,c-1)
        c > 0
            && (canonical_fence(r, c - 1) == Some(FenceOrientation::Vertical)
                || r > 0 && canonical_fence(r - 1, c - 1) == Some(FenceOrientation::Vertical))
    };
    let horizontal_fence_top = |r: usize, c: usize| {
        r > 0
            && (canonical_fence(r - 1, c) == Some(FenceOrientation::Horizontal)
                || c > 0 && canonical_fence(r - 1, c - 1) == Some(FenceOrientation::Horizontal))
    };

    let width = horizontal_label.len();
    let height = vertical_label.len();
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
            out.push(' ');
            out.push(in_box_label(r, c));
            out.push(' ');
        }
        out.push('│');
        out.push(' ');
        out.push(vertical_label[r]);
        out.push('\n');
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
    // Draw the horizontal labels
    for c in 0..width {
        out.push_str(if c == 0 { "  " } else { "   " });
        out.push(horizontal_label[c]);
    }
    out
}
