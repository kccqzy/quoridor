use std::rc::Rc;

const BOARD_WIDTH: u8 = 9;
const BOARD_HEIGHT: u8 = 9;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FenceOrientation {
    Vertical = 0,
    Horizontal = 1,
}

pub fn draw_unicode_box(
    horizontal_label: &[char], vertical_label: &[char], in_box_label: impl Fn(u8, u8) -> char,
    canonical_fence: impl Fn(u8, u8) -> Option<FenceOrientation>,
) -> String {
    let vertical_fence_left = |r: u8, c: u8| {
        // A vertical fence left of (r,c) exists iff canonical fence (r,c-1) or
        // (r-1,c-1).
        c > 0
            && (r < BOARD_HEIGHT - 1
                && canonical_fence(r, c - 1) == Some(FenceOrientation::Vertical)
                || r > 0 && canonical_fence(r - 1, c - 1) == Some(FenceOrientation::Vertical))
    };
    let horizontal_fence_top = |r: u8, c: u8| {
        // A horizontal fence on top of (r,c) exists iff canonical fence (r-1,c) or
        // (r-1,c-1).
        r > 0
            && (c < BOARD_WIDTH - 1
                && canonical_fence(r - 1, c) == Some(FenceOrientation::Horizontal)
                || c > 0 && canonical_fence(r - 1, c - 1) == Some(FenceOrientation::Horizontal))
    };

    let width = horizontal_label.len() as u8;
    let height = vertical_label.len() as u8;
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
        out.push(vertical_label[r as usize]);
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
        out.push(horizontal_label[c as usize]);
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
    Move([u8; 2]),
    Fence(([u8; 2], FenceOrientation)),
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn iterator() -> impl Iterator<Item = Direction> {
        use self::Direction::*;
        [North, South, East, West].iter().copied()
    }
    fn perpendicular_iterator(self) -> impl Iterator<Item = Direction> {
        use self::Direction::*;
        match self {
            North | South => [East, West].iter().copied(),
            East | West => [North, South].iter().copied(),
        }
    }
}

#[derive(Debug, Clone)]
struct FenceState {
    /// The fences placed as a bit set.
    fences: [u64; 2],
    /// The number of fences remaining for each player.
    fences_remaining: [u8; 2],
}

impl FenceState {
    fn new() -> FenceState {
        FenceState { fences: [0; 2], fences_remaining: [10; 2] }
    }
    fn get(&self, [r, c]: [u8; 2]) -> Option<FenceOrientation> {
        assert!(r < BOARD_HEIGHT - 1 && c < BOARD_WIDTH - 1, "location out of bounds {},{}", r, c);
        let index = r * (BOARD_WIDTH - 1) + c;
        if self.fences[FenceOrientation::Vertical as usize] & (1 << index) != 0 {
            Some(FenceOrientation::Vertical)
        } else if self.fences[FenceOrientation::Horizontal as usize] & (1 << index) != 0 {
            Some(FenceOrientation::Horizontal)
        } else {
            None
        }
    }
    fn remaining(&self, player: Player) -> u8 {
        self.fences_remaining[player as usize]
    }
    fn has(&self, loc @ [fr, fc]: [u8; 2], o: FenceOrientation) -> bool {
        fr < BOARD_HEIGHT - 1 && fc < BOARD_WIDTH - 1 && self.get(loc) == Some(o)
    }
    fn try_place_fence(
        this: &mut Rc<Self>, player: Player, (loc @ [r, c], o): ([u8; 2], FenceOrientation),
    ) -> std::result::Result<(), &'static str> {
        if !(r < BOARD_HEIGHT - 1 && c < BOARD_WIDTH - 1) {
            Err("cannot place fences on a border or outside the boundary")
        } else if this.remaining(player) == 0 {
            Err("no more remaining fences")
        } else if this.get(loc).is_some() {
            Err("cannot place fence on top of or intersecting an existing fence")
        } else {
            let mut potential_overlap = [loc, loc];
            potential_overlap[0][o as usize] += 1;
            potential_overlap[1][o as usize] = potential_overlap[1][o as usize].wrapping_add(255);
            if potential_overlap.into_iter().any(|floc| this.has(floc, o)) {
                Err("cannot place fence that overlaps with an existing fence")
            } else {
                let inner: &mut Self = Rc::make_mut(this);
                let index = r * (BOARD_WIDTH - 1) + c;
                inner.fences[o as usize] |= 1 << index;
                inner.fences_remaining[player as usize] -= 1;
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct GameState {
    fence_state: Rc<FenceState>,
    location: [[u8; 2]; 2],
}

impl GameState {
    pub fn new() -> GameState {
        GameState { fence_state: Rc::new(FenceState::new()), location: [[8, 4], [0, 4]] }
    }

    pub fn draw(&self, show_next_moves: bool) -> String {
        let vertical_label: [char; BOARD_HEIGHT as usize] =
            ['9', '8', '7', '6', '5', '4', '3', '2', '1'];
        let horizontal_label: [char; BOARD_WIDTH as usize] =
            ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'];
        let player_1_symbol = '■';
        let player_2_symbol = '●';
        let player_1_next_symbol = '□';
        let player_2_next_symbol = '○';
        let player_1_next =
            if show_next_moves { self.valid_moves(Player::Player1) } else { vec![] };
        let player_2_next =
            if show_next_moves { self.valid_moves(Player::Player2) } else { vec![] };
        draw_unicode_box(
            &horizontal_label,
            &vertical_label,
            |r, c| {
                if [r, c] == self.location[0] {
                    player_1_symbol
                } else if [r, c] == self.location[1] {
                    player_2_symbol
                } else if player_1_next.iter().any(|&l| l == [r, c]) {
                    player_1_next_symbol
                } else if player_2_next.iter().any(|&l| l == [r, c]) {
                    player_2_next_symbol
                } else {
                    ' '
                }
            },
            |r, c| self.fence_state.get([r, c]),
        )
    }

    pub fn is_game_complete(&self) -> bool {
        self.location[0][0] == 0 || self.location[1][0] == BOARD_HEIGHT - 1
    }

    fn is_move_fence_free(
        &self, old_loc @ [r, c]: [u8; 2], direction: Direction,
    ) -> Option<[u8; 2]> {
        let [dr, dc] = match direction {
            Direction::North => [255, 0],
            Direction::South => [1, 0],
            Direction::West => [0, 255],
            Direction::East => [0, 1],
        };
        let new_loc = [r.wrapping_add(dr), c.wrapping_add(dc)];

        let blocking_fence_orientation = match direction {
            Direction::North | Direction::South => FenceOrientation::Horizontal,
            Direction::East | Direction::West => FenceOrientation::Vertical,
        };
        // Along the longitudinal axis of the fence, the move is
        // blocked if the top-left corner of the fence is the same
        // as the old location, or just one less than the old
        // location.
        //
        // Along the transverse axis of the fence, the move is
        // blocked if the top-left corner of the fence is the same
        // as the minimum of old and new location.
        let mut blocking_fences: [[u8; 2]; 2] = [[0; 2]; 2];
        let fo = blocking_fence_orientation;
        blocking_fences[0][fo as usize] = old_loc[fo as usize];
        blocking_fences[0][1 - fo as usize] =
            std::cmp::min(old_loc[1 - fo as usize], new_loc[1 - fo as usize]);
        blocking_fences[1][fo as usize] = old_loc[fo as usize].wrapping_add(255);
        blocking_fences[1][1 - fo as usize] = blocking_fences[0][1 - fo as usize];
        let is_fence_free = !blocking_fences.into_iter().any(|floc| self.fence_state.has(floc, fo));

        if is_fence_free {
            Some(new_loc)
        } else {
            None
        }
    }

    pub fn valid_moves(&self, player: Player) -> Vec<[u8; 2]> {
        // The move rules are the follows: generally the pawn can move
        // one space in any cardinal direction, except when blocked by a
        // fence. The pawn can jump over the other player. If there is a
        // fence behind the other player, the pawn can jump diagonally,
        // if there is no fence between the other player and the
        // location of the jump.
        let loc = self.location[player as usize];
        let mut rv = vec![];

        for dir in Direction::iterator() {
            if let Some(new_loc) = self.is_move_fence_free(loc, dir) {
                if self.location[1 - player as usize] != new_loc {
                    rv.push(new_loc);
                } else {
                    // The other player is occupying the space. Try jumping forward.
                    if let Some(final_loc) = self.is_move_fence_free(new_loc, dir) {
                        rv.push(final_loc);
                    } else {
                        // We can jump in the perpendicular direction.
                        for pdir in dir.perpendicular_iterator() {
                            if let Some(final_loc) = self.is_move_fence_free(new_loc, pdir) {
                                rv.push(final_loc);
                            }
                        }
                    }
                }
            }
        }

        rv
    }

    pub fn perform_action(
        &self, player: Player, action: Action,
    ) -> std::result::Result<Self, &'static str> {
        if self.is_game_complete() {
            return Err("cannot move because game is over");
        }
        match action {
            Action::Move(new_loc) => {
                if self.valid_moves(player).iter().any(|&valid_loc| valid_loc == new_loc) {
                    let mut new_state = self.clone();
                    new_state.location[player as usize] = new_loc;
                    Ok(new_state)
                } else {
                    Err("the location of the move is not allowed")
                }
            }
            Action::Fence(new_fence) => {
                let mut new_state = self.clone();
                FenceState::try_place_fence(&mut new_state.fence_state, player, new_fence)?;
                // TODO: reachability check

                Ok(new_state)
            }
        }
    }
}
