use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::Display;
use std::rc::Rc;

const BOARD_WIDTH: u8 = 9;
const BOARD_HEIGHT: u8 = 9;

const VERTICAL_LABEL: [char; BOARD_HEIGHT as usize] = ['9', '8', '7', '6', '5', '4', '3', '2', '1'];
const HORIZONTAL_LABEL: [char; BOARD_WIDTH as usize] =
    ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'];
const PLAYER_1_SYMBOL: char = '■';
const PLAYER_2_SYMBOL: char = '●';
const PLAYER_1_NEXT_SYMBOL: char = '□';
const PLAYER_2_NEXT_SYMBOL: char = '○';

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum FenceOrientation {
    Vertical = 0,
    Horizontal = 1,
}

impl FenceOrientation {
    pub fn other(&self) -> Self {
        match *self {
            FenceOrientation::Horizontal => FenceOrientation::Vertical,
            FenceOrientation::Vertical => FenceOrientation::Horizontal,
        }
    }
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
mod draw_tests {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Player {
    Player1 = 0,
    Player2 = 1,
}

impl Player {
    pub fn other(&self) -> Self {
        match *self {
            Player::Player1 => Player::Player2,
            Player::Player2 => Player::Player1,
        }
    }
    pub fn iterator() -> impl Iterator<Item = Player> {
        [Player::Player1, Player::Player2].iter().copied()
    }
}

impl Display for Player {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Player {} ({})", 1 + *self as usize, match *self {
            Player::Player1 => PLAYER_1_SYMBOL,
            Player::Player2 => PLAYER_2_SYMBOL,
        }))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Fence([u8; 2], FenceOrientation);

#[derive(Debug, Clone, Copy)]
pub enum Action {
    Move([u8; 2]),
    Fence(Fence),
}

impl TryFrom<&[u8]> for Action {
    type Error = &'static str;
    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        match value {
            [first @ b'a'..=b'i', second @ b'1'..=b'9'] => {
                let c = first - b'a';
                let r = BOARD_HEIGHT - (second - b'1') - 1;
                Ok(Action::Move([r, c]))
            }
            [first @ b'a'..=b'i', second @ b'1'..=b'9', third @ (b'h' | b'v')] => {
                let c = first - b'a';
                let r = BOARD_HEIGHT - (second - b'1') - 1;
                let fo = if *third == b'h' {
                    FenceOrientation::Horizontal
                } else {
                    FenceOrientation::Vertical
                };
                Ok(Action::Fence(Fence([r, c], fo)))
            }
            _ => Err("unrecognized command"),
        }
    }
}

impl std::str::FromStr for Action {
    type Err = &'static str;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        s.as_bytes().try_into()
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Action::Move([r, c]) => {
                let out = [b'a' + c, b'1' + (BOARD_HEIGHT - 1 - r)];
                f.write_str(std::str::from_utf8(&out).unwrap())
            }
            Action::Fence(Fence([r, c], o)) => {
                let out = [b'a' + c, b'1' + (BOARD_HEIGHT - 1 - r), match o {
                    FenceOrientation::Horizontal => b'h',
                    FenceOrientation::Vertical => b'v',
                }];
                f.write_str(std::str::from_utf8(&out).unwrap())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ActionError {
    FenceOutsideBoundary,
    NoRemainingFence,
    OnTopOfExistingFence,
    IntersectExistingFence,
    OverlapExistingFence,
    GameOver,
    InvalidMove,
    PlayerHasNoPath(Player),
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

impl TryFrom<&[u8]> for Direction {
    type Error = &'static str;
    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        use self::Direction::*;
        if value == b"north" || value == b"n" {
            Ok(North)
        } else if value == b"south" || value == b"s" {
            Ok(South)
        } else if value == b"east" || value == b"e" {
            Ok(East)
        } else if value == b"west" || value == b"w" {
            Ok(West)
        } else {
            Err("unrecognized cardinal direction")
        }
    }
}

impl std::str::FromStr for Direction {
    type Err = &'static str;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        s.as_bytes().try_into()
    }
}

/// A path of coordinates from destination to source.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Path(Vec<[u8; 2]>);

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for &loc in self.0.iter().rev() {
            write!(f, " {}", Action::Move(loc))?;
        }
        Ok(())
    }
}

/// The fences placed as a bit set. This is an array of two 64-bit integers. The
/// array index represents the fence orientation, and the array value represents
/// the location. Since the board is 9x9 and each fence must necessarily span
/// two places, the placeable fence locations are in the top 8x8 grid.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct FenceSet([u64; 2]);

impl FenceSet {
    fn get(&self, [r, c]: [u8; 2]) -> Option<FenceOrientation> {
        assert!(r < BOARD_HEIGHT - 1 && c < BOARD_WIDTH - 1, "location out of bounds {},{}", r, c);
        let index = r * (BOARD_WIDTH - 1) + c;
        if self.0[FenceOrientation::Vertical as usize] & (1 << index) != 0 {
            Some(FenceOrientation::Vertical)
        } else if self.0[FenceOrientation::Horizontal as usize] & (1 << index) != 0 {
            Some(FenceOrientation::Horizontal)
        } else {
            None
        }
    }

    fn has(&self, loc @ [fr, fc]: [u8; 2], o: FenceOrientation) -> bool {
        fr < BOARD_HEIGHT - 1 && fc < BOARD_WIDTH - 1 && self.get(loc) == Some(o)
    }

    /// Whether or not the move from a particular old location in a direction is
    /// obstructed by a fence. If it is, return None. Otherwise return the new
    /// location.
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

        // The border of the grid is an implicit fence.
        if !GameState::is_within_bound(new_loc) {
            return None;
        }

        let (fo, blocking_fences) = Self::blocking_fences(old_loc, new_loc);
        let is_fence_free = !blocking_fences.into_iter().any(|floc| self.has(floc, fo));

        if is_fence_free {
            Some(new_loc)
        } else {
            None
        }
    }

    /// Find all the potentially blocking fences between two adjacent locations.
    /// This does not check whether those fences exist, can be placed, or indeed
    /// even valid.
    fn blocking_fences(old_loc: [u8; 2], new_loc: [u8; 2]) -> (FenceOrientation, [[u8; 2]; 2]) {
        if old_loc[0] == new_loc[0] {
            debug_assert_eq!(old_loc[1].abs_diff(new_loc[1]), 1);
        } else if old_loc[1] == new_loc[1] {
            debug_assert_eq!(old_loc[0].abs_diff(new_loc[0]), 1);
        } else {
            debug_assert!(false, "locations must be in the same row or column");
        }

        let fo = if old_loc[0] == new_loc[0] {
            FenceOrientation::Vertical
        } else {
            FenceOrientation::Horizontal
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
        blocking_fences[0][fo as usize] = old_loc[fo as usize];
        blocking_fences[0][1 - fo as usize] =
            std::cmp::min(old_loc[1 - fo as usize], new_loc[1 - fo as usize]);
        blocking_fences[1][fo as usize] = old_loc[fo as usize].wrapping_add(255);
        blocking_fences[1][1 - fo as usize] = blocking_fences[0][1 - fo as usize];
        (fo, blocking_fences)
    }

    fn valid_moves_from(&self, loc: [u8; 2], other_player_loc: Option<[u8; 2]>) -> Vec<[u8; 2]> {
        // The move rules are the follows: generally the pawn can move
        // one space in any cardinal direction, except when blocked by a
        // fence. The pawn can jump over the other player. If there is a
        // fence behind the other player, the pawn can jump diagonally,
        // if there is no fence between the other player and the
        // location of the jump.
        let mut rv = vec![];

        for dir in Direction::iterator() {
            if let Some(new_loc) = self.is_move_fence_free(loc, dir) {
                let occupied_by_other_player = other_player_loc.map_or(false, |l| l == new_loc);
                if !occupied_by_other_player {
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

    fn shortest_distance(&self, from: [u8; 2], goal_r: u8) -> Option<Path> {
        struct HeapItem {
            cur: [u8; 2],
            prev: [u8; 2],
            dist_from_start: u8,
            est_dist_to_finish: u8,
        }
        impl PartialEq for HeapItem {
            fn eq(&self, other: &Self) -> bool {
                self.dist_from_start + self.est_dist_to_finish
                    == other.dist_from_start + other.est_dist_to_finish
            }
        }
        impl Eq for HeapItem {}
        impl PartialOrd for HeapItem {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                (other.dist_from_start + other.est_dist_to_finish)
                    .partial_cmp(&(self.dist_from_start + self.est_dist_to_finish))
            }
        }
        impl Ord for HeapItem {
            fn cmp(&self, other: &Self) -> Ordering {
                (other.dist_from_start + other.est_dist_to_finish)
                    .cmp(&(self.dist_from_start + self.est_dist_to_finish))
            }
        }

        let mut steps = vec![];
        let mut visited = [[false; BOARD_WIDTH as usize]; BOARD_HEIGHT as usize];
        let mut queue = std::collections::BinaryHeap::from([HeapItem {
            cur: from,
            prev: from,
            dist_from_start: 0,
            est_dist_to_finish: from[0].abs_diff(goal_r),
        }]);
        visited[from[0] as usize][from[1] as usize] = true;
        while let Some(item) = queue.pop() {
            steps.push((item.prev, item.cur));
            if item.est_dist_to_finish == 0 {
                // Found. We just need to reconstruct the path.
                let mut path = vec![item.cur];
                let mut cur = item.cur;
                for (prev, next) in steps.into_iter().skip(1).rev() {
                    if cur == next {
                        path.push(prev);
                        cur = prev;
                    }
                }
                assert_eq!(*path.last().unwrap(), from);
                assert_eq!(path.len() - 1, item.dist_from_start as usize);
                return Some(Path(path));
            }
            for new_loc in self.valid_moves_from(item.cur, None).into_iter() {
                let visited = &mut visited[new_loc[0] as usize][new_loc[1] as usize];
                if !*visited {
                    *visited = true;
                    let new_item = HeapItem {
                        cur: new_loc,
                        prev: item.cur,
                        dist_from_start: item.dist_from_start + 1,
                        est_dist_to_finish: new_loc[0].abs_diff(goal_r),
                    };
                    queue.push(new_item);
                }
            }
        }
        None
    }

    fn try_place_fence(
        &self, Fence(loc @ [r, c], o): Fence,
    ) -> std::result::Result<Self, ActionError> {
        if !(r < BOARD_HEIGHT - 1 && c < BOARD_WIDTH - 1) {
            Err(ActionError::FenceOutsideBoundary)
        } else if let Some(existing) = self.get(loc) {
            Err(if existing == o {
                ActionError::OnTopOfExistingFence
            } else {
                ActionError::IntersectExistingFence
            })
        } else {
            let mut potential_overlap = [loc, loc];
            potential_overlap[0][o as usize] += 1;
            potential_overlap[1][o as usize] = potential_overlap[1][o as usize].wrapping_add(255);
            if potential_overlap.into_iter().any(|floc| self.has(floc, o)) {
                Err(ActionError::OverlapExistingFence)
            } else {
                let mut rv = self.clone();
                let index = r * (BOARD_WIDTH - 1) + c;
                rv.0[o as usize] |= 1 << index;
                Ok(rv)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FenceState {
    /// The fences placed as a bit set.
    fences: FenceSet,
    /// The number of fences remaining for each player.
    fences_remaining: [u8; 2],
}

impl FenceState {
    fn new() -> FenceState {
        FenceState { fences: FenceSet([0; 2]), fences_remaining: [10; 2] }
    }

    fn remaining(&self, player: Player) -> u8 {
        self.fences_remaining[player as usize]
    }

    fn try_place_fence(&self, player: Player, f: Fence) -> std::result::Result<Self, ActionError> {
        if self.remaining(player) == 0 {
            Err(ActionError::NoRemainingFence)
        } else {
            let fences = self.fences.try_place_fence(f)?;
            let mut fences_remaining = self.fences_remaining;
            fences_remaining[player as usize] -= 1;
            Ok(FenceState { fences, fences_remaining })
        }
    }
}

fn max_by_key_with_ties<It: Iterator, B: Ord, F>(it: It, mut f: F) -> Vec<It::Item>
where
    F: FnMut(&It::Item) -> B,
{
    let mut rv = vec![];
    for el in it {
        match rv.last() {
            None => rv.push(el),
            Some(top) => match f(top).cmp(&f(&el)) {
                Ordering::Less => {
                    rv.clear();
                    rv.push(el);
                }
                Ordering::Equal => rv.push(el),
                Ordering::Greater => {}
            },
        };
    }
    rv
}

fn min_by_key_with_ties<It: Iterator, B: Ord, F>(it: It, mut f: F) -> Vec<It::Item>
where
    F: FnMut(&It::Item) -> B,
{
    max_by_key_with_ties(it, |e| std::cmp::Reverse(f(e)))
}

#[derive(Debug, Clone)]
pub struct GameState {
    fence_state: Rc<FenceState>,
    location: [[u8; 2]; 2],
    shortest_path: [Rc<Path>; 2],
}

impl PartialEq for GameState {
    fn eq(&self, other: &Self) -> bool {
        self.fence_state == other.fence_state && self.location == other.location
    }
}

impl Eq for GameState {}

impl GameState {
    fn recalc_shortest_path(mut self) -> Result<Self, ActionError> {
        for player in Player::iterator() {
            let actual_shortest_path = self
                .fence_state
                .fences
                .shortest_distance(self.location[player as usize], Self::goal_r_for_player(player))
                .ok_or(ActionError::PlayerHasNoPath(player))?;

            if self.shortest_path[player as usize].0 != actual_shortest_path.0 {
                *Rc::make_mut(&mut self.shortest_path[player as usize]) = actual_shortest_path;
            }
        }
        Ok(self)
    }

    pub fn new() -> GameState {
        GameState {
            fence_state: Rc::new(FenceState::new()),
            location: [[8, 4], [0, 4]],
            shortest_path: Default::default(),
        }
        .recalc_shortest_path()
        .unwrap()
    }

    pub fn draw(&self, show_next_moves: bool) -> String {
        let player_1_next =
            if show_next_moves { self.valid_moves(Player::Player1) } else { vec![] };
        let player_2_next =
            if show_next_moves { self.valid_moves(Player::Player2) } else { vec![] };
        draw_unicode_box(
            &HORIZONTAL_LABEL,
            &VERTICAL_LABEL,
            |r, c| {
                if [r, c] == self.location[0] {
                    PLAYER_1_SYMBOL
                } else if [r, c] == self.location[1] {
                    PLAYER_2_SYMBOL
                } else if player_1_next.iter().any(|&l| l == [r, c]) {
                    PLAYER_1_NEXT_SYMBOL
                } else if player_2_next.iter().any(|&l| l == [r, c]) {
                    PLAYER_2_NEXT_SYMBOL
                } else {
                    ' '
                }
            },
            |r, c| self.fence_state.fences.get([r, c]),
        )
    }

    pub fn is_game_complete(&self) -> bool {
        self.location[0][0] == 0 || self.location[1][0] == BOARD_HEIGHT - 1
    }

    pub fn valid_moves(&self, player: Player) -> Vec<[u8; 2]> {
        let loc = self.location[player as usize];
        let other_loc = Some(self.location[1 - player as usize]);
        self.fence_state.fences.valid_moves_from(loc, other_loc)
    }

    fn goal_r_for_player(player: Player) -> u8 {
        match player {
            Player::Player1 => 0,
            Player::Player2 => BOARD_HEIGHT - 1,
        }
    }

    pub fn perform_action(
        &self, player: Player, action: Action,
    ) -> std::result::Result<Self, ActionError> {
        if self.is_game_complete() {
            return Err(ActionError::GameOver);
        }
        match action {
            Action::Move(new_loc) => {
                if self.valid_moves(player).iter().any(|&valid_loc| valid_loc == new_loc) {
                    let mut new_state = GameState {
                        fence_state: self.fence_state.clone(),
                        location: self.location,
                        shortest_path: self.shortest_path.clone(),
                    };
                    new_state.location[player as usize] = new_loc;
                    Ok(new_state
                        .recalc_shortest_path()
                        .expect("moving should not cause a path to disappear"))
                } else {
                    Err(ActionError::InvalidMove)
                }
            }
            Action::Fence(new_fence) => GameState {
                fence_state: Rc::new(FenceState::try_place_fence(
                    &self.fence_state,
                    player,
                    new_fence,
                )?),
                location: self.location,
                shortest_path: self.shortest_path.clone(),
            }
            .recalc_shortest_path(),
        }
    }

    pub fn is_within_bound(loc: [u8; 2]) -> bool {
        loc[0] < BOARD_HEIGHT && loc[1] < BOARD_WIDTH
    }

    pub fn parse_move_for_player(
        &self, player: Player, cmd: &str,
    ) -> std::result::Result<Action, String> {
        let mut loc = self.location[player as usize];

        let cmd_str: &[u8] = cmd.as_bytes();
        if let Ok(dir) = Direction::try_from(cmd_str) {
            // Cardinal directions
            let [dr, dc] = match dir {
                Direction::North => [255, 0],
                Direction::South => [1, 0],
                Direction::West => [0, 255],
                Direction::East => [0, 1],
            };
            loc[0] = loc[0].wrapping_add(dr);
            loc[1] = loc[1].wrapping_add(dc);
            if Self::is_within_bound(loc) {
                Ok(Action::Move(loc))
            } else {
                Err(format!("cannot go in direction {:?}", dir))
            }
        } else {
            // Algebraic notation
            Action::try_from(cmd_str).map_err(|e| e.into())
        }
    }

    fn find_obstructing_fence_for_path(path: &Path) -> BTreeSet<Fence> {
        path.0
            .windows(2)
            .flat_map(|step| {
                let (fo, [f1, f2]) = FenceSet::blocking_fences(step[0], step[1]);
                [Fence(f1, fo), Fence(f2, fo)]
            })
            .collect()
    }
}

struct FenceStateSimulation<'a> {
    gs: &'a GameState,
    /// Future shortest paths. This is a map from a fence set (which must be
    /// supersets of the current fence set) to player shortest paths.
    future_shortest_paths: BTreeMap<FenceSet, [Option<Path>; 2]>,
}

impl<'a> FenceStateSimulation<'a> {
    fn from_game_state(gs: &GameState) -> FenceStateSimulation {
        FenceStateSimulation { gs, future_shortest_paths: BTreeMap::new() }
    }

    fn evaluate_fences(
        &mut self, fs: &[Fence],
    ) -> std::result::Result<[Option<Path>; 2], ActionError> {
        let new_fence_set = fs.iter().try_fold(self.gs.fence_state.fences.clone(), |fs, f| {
            FenceSet::try_place_fence(&fs, *f)
        })?;
        Ok(self
            .future_shortest_paths
            .entry(new_fence_set)
            .or_insert_with_key(|k| {
                [Player::Player1, Player::Player2].map(|player| {
                    k.shortest_distance(
                        self.gs.location[player as usize],
                        GameState::goal_r_for_player(player),
                    )
                })
            })
            .clone())
    }

    fn evaluate_fence_for_players(
        &mut self, fs: &[Fence], cur_player: Player, cur_player_fence_count: usize,
        other_player_fence_count: usize,
    ) -> std::result::Result<[Option<Path>; 2], ActionError> {
        assert_eq!(fs.len(), cur_player_fence_count + other_player_fence_count);
        for player in Player::iterator() {
            if (self.gs.fence_state.remaining(player) as usize)
                < if player == cur_player {
                    cur_player_fence_count
                } else {
                    other_player_fence_count
                }
            {
                return Err(ActionError::NoRemainingFence);
            }
        }
        self.evaluate_fences(fs)
    }

    /// Get potential defending fences given proposed opponent fences, so
    /// that the opponent fences are not possible.
    fn get_potential_defending_fences(
        &mut self, future_path: &Path, player: Player, proposed_opponent_fences: &[Fence],
        rv: &mut BTreeSet<Fence>,
    ) {
        // The defending fence can block off an alternative route, or it simply
        // prevents the current fence from being placed by having an overlap or
        // at the same location.
        // This is the former.
        for f in GameState::find_obstructing_fence_for_path(future_path) {
            if !rv.contains(&f) {
                // Check that the fence f will result in no path.
                let mut all_fences = proposed_opponent_fences.to_vec();
                all_fences.push(f);
                if let Ok(paths) = self.evaluate_fence_for_players(
                    &all_fences,
                    player,
                    1,
                    proposed_opponent_fences.len(),
                ) {
                    if paths[player as usize].is_none() {
                        rv.insert(f);
                    }
                }
            }
        }

        // This is the latter.
        for f in proposed_opponent_fences.iter().flat_map(|&f @ Fence(_, o)| {
            let mut fences = [f; 3];
            // The first one simply rotates the given fence.
            fences[0].1 = o.other();
            // The second one is the given fence translated along the longitudinal direction
            // by 1.
            fences[1].0[o as usize] += 1;
            // The third one is the given fence translated along the longitudinal direction
            // by -1.
            fences[2].0[o as usize] = fences[2].0[o as usize].wrapping_add(255);
            fences
        }) {
            rv.insert(f);
        }
    }

    /// Produce an evaluation of a single obstructing fence, i.e. a fence
    /// that would cause the current player's shortest path to
    /// increase.
    fn obstructing_fence_evaluation(
        &mut self, current_path: &Path, player: Player, cur_player_extra_fence: Option<Fence>,
    ) -> Vec<(Fence, Path)> {
        GameState::find_obstructing_fence_for_path(current_path)
            .into_iter()
            .filter_map(|f| {
                match cur_player_extra_fence {
                    None => self.evaluate_fence_for_players(&[f], player, 0, 1),
                    Some(ef) => self.evaluate_fence_for_players(&[f, ef], player, 1, 1),
                }
                .ok()
                .and_then(|mut pp| std::mem::take(&mut pp[player as usize]))
                .map(|p| (f, p))
            })
            .collect::<Vec<_>>()
    }

    /// Produce an evaluation of two obstructing fences, i.e. two fences
    /// that together would cause the current player's shortest path
    /// to increase.
    fn obstructing_two_fence_evaluation(
        &mut self, player: Player, fences: Vec<(Fence, Path)>,
        cur_player_extra_fence: Option<Fence>,
    ) -> BTreeMap<(Fence, Fence), Path> {
        let mut rv: BTreeMap<(Fence, Fence), Path> = BTreeMap::new();

        for (f1, path) in fences.into_iter() {
            for f2 in GameState::find_obstructing_fence_for_path(&path) {
                let two_fences = match f1.cmp(&f2) {
                    Ordering::Less => (f1, f2),
                    Ordering::Equal => continue,
                    Ordering::Greater => (f2, f1),
                };
                rv.extend({
                    match cur_player_extra_fence {
                        None => self.evaluate_fence_for_players(&[f1, f2], player, 0, 2),
                        Some(ef) => self.evaluate_fence_for_players(&[f1, f2, ef], player, 1, 2),
                    }
                    .ok()
                    .and_then(|mut pp| std::mem::take(&mut pp[player as usize]))
                    .map(|p| (two_fences, p))
                });
            }
        }
        rv
    }

    fn evaluate_defending_fences<K, R: IntoIterator<Item = (K, Path)>>(
        &mut self, player: Player, defend: BTreeSet<Fence>, cur_worst_path: &Path,
        cur_orig_path: &Path, eval_fn: impl Fn(&mut Self, &Path, Fence) -> R,
    ) -> Vec<(Fence, (K, Path))> {
        let defend = defend
            .into_iter()
            .filter_map(|df| {
                self.evaluate_fence_for_players(&[df], player, 1, 0)
                    // Check that the fence can actually be placed (used to be in
                    // `get_potential_defending_fences`)
                    .ok()
                    // Check that the current player still has a path
                    .and_then(|mut pp| std::mem::take(&mut pp[player as usize]))
                    // Check that the fence doesn't cause the player's shortest path to lengthen
                    .filter(|p| p.0.len() == cur_orig_path.0.len())
                    .and_then(|p| {
                        eval_fn(self, &p, df)
                            .into_iter()
                            .max_by_key(|(_, p)| p.0.len())
                            .map(|v| (df, v))
                    })
            })
            .filter(|(_, (_, p))| p.0.len() < cur_worst_path.0.len());
        min_by_key_with_ties(defend, |(_, (_, p))| p.0.len())
    }
}

impl GameState {
    pub fn produce_info(&self, rv: &mut dyn std::io::Write) -> std::io::Result<()> {
        let mut sim = FenceStateSimulation::from_game_state(self);

        for player in Player::iterator() {
            let valid_moves = self.valid_moves(player);
            write!(rv, "{} valid moves:", player)?;
            for m in valid_moves.into_iter() {
                write!(rv, " {}", Action::Move(m))?;
            }
            writeln!(rv)?;

            let cur_player_shortest = &self.shortest_path[player as usize];
            writeln!(rv, "{} shortest path:{}", player, cur_player_shortest)?;

            // Find the fences that would result in the biggest increase in
            // shortest distance.
            //
            // Note that we first collect all possible fences. This is because
            // later on, when calculating the two worst fences, each individual
            // among the pair might not be the worst.
            let fences = sim.obstructing_fence_evaluation(cur_player_shortest, player, None);
            let worst_fences = max_by_key_with_ties(fences.iter(), |(_, p)| p.0.len());

            match worst_fences.split_first() {
                Some(((f, path), tail)) if path.0.len() > cur_player_shortest.0.len() => {
                    writeln!(
                        rv,
                        "Fence for {} that causes the most increase in shortest distance: {} \
                         (increase from {} to {}):{}",
                        player,
                        Action::Fence(*f),
                        cur_player_shortest.0.len() - 1,
                        path.0.len() - 1,
                        path
                    )?;
                    let mut defend = BTreeSet::new();
                    sim.get_potential_defending_fences(path, player, &[*f], &mut defend);
                    for (f, path) in tail {
                        writeln!(rv, "Or fence {}:{}", Action::Fence(*f), path)?;
                        sim.get_potential_defending_fences(path, player, &[*f], &mut defend);
                    }

                    // Evaluate defending fences.
                    for (df, (wf, p)) in sim.evaluate_defending_fences(
                        player,
                        defend,
                        path,
                        cur_player_shortest,
                        |s, p, df| s.obstructing_fence_evaluation(p, player, Some(df)),
                    ) {
                        writeln!(
                            rv,
                            "Potential defending fence {} because afterwards, the worst fence \
                             (e.g. {}) will make the shortest distance {} (currently {}), better \
                             than {}",
                            Action::Fence(df),
                            Action::Fence(wf),
                            p.0.len() - 1,
                            cur_player_shortest.0.len() - 1,
                            path.0.len() - 1
                        )?;
                    }
                }
                _ =>
                    writeln!(rv, "No fence for {} can cause increase in shortest distance", player)?,
            }

            let two_fences = sim.obstructing_two_fence_evaluation(player, fences, None);
            let worst_two_fences = max_by_key_with_ties(two_fences.into_iter(), |(_, p)| p.0.len());

            match worst_two_fences.split_first() {
                Some((((f1, f2), path), tail)) if path.0.len() > cur_player_shortest.0.len() => {
                    writeln!(
                        rv,
                        "Two fences for {} that causes the most increase in shortest distance: \
                         {}+{} (increase from {} to {}):{}",
                        player,
                        Action::Fence(*f1),
                        Action::Fence(*f2),
                        cur_player_shortest.0.len() - 1,
                        path.0.len() - 1,
                        path
                    )?;
                    let mut defend = BTreeSet::new();
                    sim.get_potential_defending_fences(path, player, &[*f1, *f2], &mut defend);
                    for ((f1, f2), path) in tail {
                        writeln!(
                            rv,
                            "Or fence {}+{}:{}",
                            Action::Fence(*f1),
                            Action::Fence(*f2),
                            path
                        )?;
                        sim.get_potential_defending_fences(path, player, &[*f1, *f2], &mut defend);
                    }

                    // Evaluate defending fences.
                    for (df, ((wf1, wf2), p)) in sim.evaluate_defending_fences(
                        player,
                        defend,
                        path,
                        cur_player_shortest,
                        |s, p, df| {
                            let fences = s.obstructing_fence_evaluation(p, player, Some(df));
                            s.obstructing_two_fence_evaluation(player, fences, Some(df))
                        },
                    ) {
                        writeln!(
                            rv,
                            "Potential defending fence {} because afterwards, the worst two \
                             fences (e.g. {}+{}) will make the shortest distance {} (currently \
                             {}), better than {}",
                            Action::Fence(df),
                            Action::Fence(wf1),
                            Action::Fence(wf2),
                            p.0.len() - 1,
                            cur_player_shortest.0.len() - 1,
                            path.0.len() - 1
                        )?;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod gs_tests {
    use super::*;

    fn run_trace(trace: &[&'static str]) {
        let mut gs = GameState::new();
        let mut cur_player = Player::Player1;
        let mut msg = Vec::new();
        for action_str in trace.iter() {
            let action = gs.parse_move_for_player(cur_player, action_str).unwrap();
            gs = gs.perform_action(cur_player, action).unwrap();
            gs.produce_info(&mut msg).unwrap();
            msg.clear();
            cur_player = cur_player.other();
        }
    }

    #[test]
    fn game1_trace() {
        run_trace(&["e2", "e8", "e3", "e7v", "e4", "e5v", "e5", "d7h"])
    }

    #[test]
    fn game2_trace() {
        run_trace(&[
            "e2", "e8", "e3", "e4h", "f3", "d8", "c8h", "f4v", "d9v", "c8", "a4h", "b8", "c4h",
            "e3h", "c3v", "e7v", "e5v", "d6h", "b7v", "b7", "e3", "b6", "d3", "b5", "d2", "c5",
            "e2", "c6", "f2", "d6", "g2", "e6", "g3", "g4h", "d7h", "h3v", "f2v", "d6", "g2",
            "h5h", "g1", "c6", "h1", "c7", "i1", "d7", "i2", "e7", "i3", "e8", "i4", "f8", "h4",
            "g8", "g4", "g6h", "f6v", "h8", "g5", "h7h",
        ])
    }

    #[test]
    fn game3_trace() {
        run_trace(&["e2", "e8", "e3", "e4h", "f3", "d8", "c8h", "f4v", "d9v", "c8", "f2v"])
    }

    #[test]
    fn game4_trace() {
        run_trace(&[
            "e2", "e8", "d8v", "e7", "e7h", "f7", "e3", "e4h", "g7h", "g7", "h5h", "g8", "d3",
            "c4h", "c3", "f6v", "b3", "a4h", "f5h", "g9", "c3", "e9h", "d3", "f3v", "e3", "f9",
            "e2", "e9", "f2", "e2h", "e2", "d9", "d2", "c2h", "c2", "d8", "b2", "d7", "c7h", "c7",
            "b8v", "c8", "b9h", "d8", "b1", "d9", "c1", "c9", "d1", "b9", "e1", "a9", "f1", "a8",
            "g1", "a7", "g2", "a6", "g3", "b6", "g4", "c6", "f4", "c5", "e4", "c4", "e5", "d4",
            "d5v", "c6v", "e6", "d5", "g2h", "a6h",
        ])
    }
}
