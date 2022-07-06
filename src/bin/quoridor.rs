use quoridor::ActionError;
use quoridor::GameState;
use quoridor::Player;
use std::io::Write;

fn clear_screen() -> std::io::Result<()> {
    // This is a simplified excerpt from https://github.com/watchexec/clearscreen/blob/1ff8b066fd9a9c1b105569083762e243e6254c4d/src/lib.rs#L523-L538

    // TODO: check isatty(0)

    const CSI: &[u8] = b"\x1b[";
    const CURSOR_HOME: &[u8] = b"H";
    const ERASE_SCREEN: &[u8] = b"2J";
    const ERASE_SCROLLBACK: &[u8] = b"3J";

    let mut w = std::io::stdout().lock();
    w.write_all(CSI)?;
    w.write_all(CURSOR_HOME)?;
    w.write_all(CSI)?;
    w.write_all(ERASE_SCREEN)?;
    w.write_all(CSI)?;
    w.write_all(ERASE_SCROLLBACK)?;
    w.flush()
}

fn loop_once(
    cur: &GameState, current_player: Player,
    f: impl FnOnce(std::io::StdoutLock<'_>) -> std::io::Result<()>,
) -> std::io::Result<String> {
    let mut stdout = std::io::stdout().lock();
    clear_screen()?;
    write!(stdout, "{}", cur.draw(true))?;
    if cur.is_game_complete() {
        write!(stdout, "Game Over.")?;
    }
    f(std::io::stdout().lock())?;
    writeln!(stdout)?;
    writeln!(stdout, "Current Player: {}", current_player)?;
    write!(stdout, "> ")?;
    stdout.flush()?;

    let mut input = String::new();
    std::io::stdin().read_line(&mut input)?;
    input.make_ascii_lowercase();

    Ok(input.trim().into())
}

fn main() {
    let new_game = GameState::new();

    let mut states = Vec::new();
    let mut current_player = Player::Player1;

    let mut input = loop_once(&new_game, current_player, |mut i| write!(i, "New Game")).unwrap();

    loop {
        let cur = states.last().map_or(&new_game, |(_, g)| g);

        if !input.is_ascii() {
            input =
                loop_once(cur, current_player, |mut i| write!(i, "Not an ASCII string")).unwrap();
            continue;
        }
        // Check for meta commands.
        if input == "quit" {
            break;
        }
        if input == "undo" {
            if states.is_empty() {
                input = loop_once(cur, current_player, |mut i| {
                    write!(i, "Cannot undo beyond the first action")
                })
                .unwrap();
            } else {
                current_player = current_player.other();
                states.pop();
                let cur = states.last().map_or(&new_game, |(_, g)| g);
                input = loop_once(cur, current_player, |mut i| write!(i, "Undo!")).unwrap();
            }
            continue;
        }
        if input == "info" {
            input = loop_once(cur, current_player, |mut i| cur.produce_info(&mut i)).unwrap();
            continue;
        }
        if input == "history" {
            input = loop_once(cur, current_player, |mut i| {
                for (j, (act, _)) in states.iter().enumerate() {
                    if j % 2 == 0 {
                        writeln!(i, "{:2}. {}", 1 + (j >> 1), act)?;
                    } else {
                        writeln!(i, "    {}", act)?;
                    }
                }
                Ok(())
            })
            .unwrap();
            continue;
        }
        if input == "restart" {
            states.clear();
            current_player = Player::Player1;
            input = loop_once(&new_game, current_player, |mut i| write!(i, "Restarted")).unwrap();
            continue;
        }
        match cur.parse_move_for_player(current_player, &input) {
            Err(msg) => {
                input = loop_once(cur, current_player, |mut i| {
                    write!(i, "I don't recognize that: {}", msg)
                })
                .unwrap();
            }
            Ok(act) => match cur.perform_action(current_player, act) {
                Err(e) => {
                    input = loop_once(cur, current_player, |mut i| {
                        write!(i, "That move was not valid: {}", match e {
                            ActionError::FenceOutsideBoundary =>
                                "the fence was on a border or outside the boundary",
                            ActionError::NoRemainingFence =>
                                "the current player has no more remaining fences",
                            ActionError::OnTopOfExistingFence =>
                                "the fence would be on top of an existing fence",
                            ActionError::IntersectExistingFence =>
                                "the fence would intersect an existing fence",
                            ActionError::OverlapExistingFence =>
                                "the fence would overlap an existing fence",
                            ActionError::GameOver => "the game is already over",
                            ActionError::InvalidMove => "the location of the move was not valid",
                            ActionError::PlayerHasNoPath(Player::Player1) =>
                                "the fence would cause the first player to have no path",
                            ActionError::PlayerHasNoPath(Player::Player2) =>
                                "the fence would cause the second player to have no path",
                        })
                    })
                    .unwrap();
                }
                Ok(new_gs) => {
                    current_player = current_player.other();
                    input = loop_once(&new_gs, current_player, |mut i| {
                        write!(i, "Last action by {}: {}", current_player, act)
                    })
                    .unwrap();
                    states.push((act, new_gs));
                }
            },
        }
    }
}
