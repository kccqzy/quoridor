use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
        // A vertical fence left of (r,c) exists iff canonical fence (r,c-1) or
        // (r-1,c-1).
        c > 0
            && (canonical_fence(r, c - 1) == Some(FenceOrientation::Vertical)
                || r > 0 && canonical_fence(r - 1, c - 1) == Some(FenceOrientation::Vertical))
    };
    let horizontal_fence_top = |r: usize, c: usize| {
        // A horizontal fence on top of (r,c) exists iff canonical fence (r-1,c) or
        // (r-1,c-1).
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
        for c in 0..width {
            // Draw the top-left corner of each cell. This may be the top-left
            // corner of the whole grid, the topmost row, the leftmost column,
            // or an interior cell.
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
            // Draw the top border of the cell.
            out.push_str(if horizontal_fence_top(r, c) { "━━━" } else { "───" });
        }
        // Draw the rightmost border of the grid at the top of each cell.
        out.push(if r == 0 {
            '┐'
        } else if horizontal_fence_top(r, width - 1) {
            '┥'
        } else {
            '┤'
        });
        out.push('\n');
        // Draw the middle of each cell.
        for c in 0..width {
            out.push(if vertical_fence_left(r, c) { '┃' } else { '│' });
            out.push(' ');
            out.push(in_box_label(r, c));
            out.push(' ');
        }
        // Draw the rightmost border of the grid in the middle. After a sapce, draw the
        // vertical labels.
        out.push('│');
        out.push(' ');
        out.push(vertical_label[r]);
        out.push('\n');
    }
    // Draw the bottommost border of the grid.
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
    // Draw the horizontal labels.
    for c in 0..width {
        out.push_str(if c == 0 { "  " } else { "   " });
        out.push(horizontal_label[c]);
    }
    out.push('\n');
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_drawing() {
        assert_eq!(
            draw_unicode_box(&['1'], &['a'], |_, _| ' ', |_, _| None),
            concat!(
                "┌───┐\n", //
                "│   │ a\n",     //
                "└───┘\n", //
                "  1\n"
            )
        );
        assert_eq!(
            draw_unicode_box(&['1', '2'], &['a'], |_, _| ' ', |_, _| None),
            concat!(
                "┌───┬───┐\n", //
                "│   │   │ a\n",           //
                "└───┴───┘\n", //
                "  1   2\n"
            )
        );
        assert_eq!(
            draw_unicode_box(&['1', '2'], &['x', 'y'], |_, _| ' ', |_, _| None),
            concat!(
                "┌───┬───┐\n", //
                "│   │   │ x\n",           //
                "├───┼───┤\n", //
                "│   │   │ y\n",           //
                "└───┴───┘\n", //
                "  1   2\n"
            )
        );
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Player {
    Player1 = 0,
    Player2 = 1,
}

#[derive(Debug, Clone, Copy)]
pub enum Action {
    Move((u8, u8)),
    Fence((u8, u8, FenceOrientation)),
}

#[derive(Debug, Clone)]
pub struct GameState {
    fences: Rc<Vec<(u8, u8, FenceOrientation)>>,
    width: u8,
    height: u8,
    location: [(u8, u8); 2],
    fences_remaining: [u8; 2],
}

impl GameState {
    pub fn new() -> GameState {
        GameState {
            width: 9,
            height: 9,
            fences: Rc::new(vec![]),
            location: [(8, 4), (0, 4)],
            fences_remaining: [10, 10],
        }
    }

    pub fn draw(&self) -> String {
        assert_eq!(self.width, 9, "cannot draw nonstandard boards");
        assert_eq!(self.height, 9, "cannot draw nonstandard boards");
        let vertical_label = ['9', '8', '7', '6', '5', '4', '3', '2', '1'];
        let horizontal_label = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'];
        let player_1_symbol = '■';
        let player_2_symbol = '○';
        draw_unicode_box(
            &horizontal_label,
            &vertical_label,
            |r, c| {
                if (r as u8, c as u8) == self.location[0] {
                    player_1_symbol
                } else if (r as u8, c as u8) == self.location[1] {
                    player_2_symbol
                } else {
                    ' '
                }
            },
            |r, c| {
                self.fences
                    .iter()
                    .find(|&&(fr, fc, _)| (fr, fc) == (r as u8, c as u8))
                    .map(|&(_, _, o)| o)
            },
        )
    }

    pub fn is_game_complete(&self) -> bool {
        self.location[0].0 == 0 || self.location[1].0 == self.height - 1
    }

    pub fn is_within_bound(&self, r: u8, c: u8) -> bool {
        return r < self.height && c < self.width;
    }

    pub fn valid_moves(&self, player: Player) -> Vec<(u8, u8)> {
        // The move rules are the follows: generally the pawn can move
        // one space in any cardinal direction, except when blocked by a
        // fence. The pawn can jump over the other player. If there is a
        // fence behind the other player, the pawn can jump diagonally,
        // if there is no fence between the other player and the
        // location of the jump.
        let (r, c) = self.location[player as usize];
        let mut rv = vec![];

        // Try move south.
        if r < self.height - 1
            && !self.fences.iter().any(|&(fr, fc, fo)| {
                fo == FenceOrientation::Horizontal && (fc == c || fc + 1 == c) && fr == r
            })
        {
            rv.push((r + 1, c));
        }

        // Try move north
        if r > 0
            && !self.fences.iter().any(|&(fr, fc, fo)| {
                fo == FenceOrientation::Horizontal && (fc == c || fc + 1 == c) && fr + 1 == r
            })
        {
            rv.push((r - 1, c));
        }

        // Try move east
        if c < self.width - 1
            && !self.fences.iter().any(|&(fr, fc, fo)| {
                fo == FenceOrientation::Vertical && (fr == r || fr + 1 == r) && fc == c
            })
        {
            rv.push((r, c + 1));
        }

        // Try move west
        if c > 0
            && !self.fences.iter().any(|&(fr, fc, fo)| {
                fo == FenceOrientation::Vertical && (fr == r || fr + 1 == r) && fc + 1 == c
            })
        {
            rv.push((r, c - 1));
        }

        // TODO: jumping

        rv
    }

    pub fn perform_action(&self, player: Player, action: Action) -> Option<Self> {
        if self.is_game_complete() {
            return None;
        }
        match action {
            Action::Move(new_loc) => {
                if self.valid_moves(player).iter().any(|&valid_loc| valid_loc == new_loc) {
                    let mut new_state = self.clone();
                    new_state.location[player as usize] = new_loc;
                    Some(new_state)
                } else {
                    None
                }
            }
            Action::Fence(new_fence @ (rr, cc, o)) => {
                // Fences cannot be placed on a border.
                if !(rr < self.height - 1 && cc < self.width - 1) {
                    None
                }
                // The player needs to have remaining fences.
                else if self.fences_remaining[player as usize] == 0 {
                    None
                }
                // Fences cannot be placed on top of or intersecting an existing fence.
                else if self.fences.iter().any(|&(fr, fc, _)| fr == rr && fc == cc) {
                    None
                }
                // Fences cannot be overlap any existing fence
                else if o == FenceOrientation::Vertical
                    && self.fences.iter().any(|&(fr, fc, fo)| {
                        fo == FenceOrientation::Vertical
                            && fc == cc
                            && (fr as i8 - rr as i8).abs() == 1
                    })
                {
                    None
                } else if o == FenceOrientation::Horizontal
                    && self.fences.iter().any(|&(fr, fc, fo)| {
                        fo == FenceOrientation::Horizontal
                            && fr == rr
                            && (fc as i8 - cc as i8).abs() == 1
                    })
                {
                    None
                } else {
                    let mut new_state = self.clone();
                    new_state.fences_remaining[player as usize] -= 1;
                    Rc::make_mut(&mut new_state.fences).push(new_fence);
                    Some(new_state)
                }
            }
        }
    }
}
