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

fn main() {
    use std::fmt::Write;
    let new_game = GameState::new();

    let mut states = Vec::new();
    let mut current_player = Player::Player1;
    let mut current_message: String = "New Game".into();

    loop {
        let cur = states.last().map_or(&new_game, |(_, g)| g);

        clear_screen().unwrap();
        std::io::stdout().lock().write_all(cur.draw(true).as_bytes()).unwrap();
        if cur.is_game_complete() {
            println!("Game Over.");
        }
        println!("{}", current_message.trim());
        println!("Current Player: {}", current_player);

        current_message.clear();

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if !input.is_ascii() {
            current_message.push_str("not an ASCII string");
            continue;
        }
        input.make_ascii_lowercase();
        let input = input.trim();
        // Check for meta commands.
        if input == "quit" {
            break;
        }
        if input == "undo" {
            if states.is_empty() {
                current_message.push_str("Cannot undo beyond the first action");
            } else {
                current_message.push_str("Undo!");
                current_player = current_player.other();
                states.pop();
            }
            continue;
        }
        if input == "info" {
            cur.produce_info(&mut current_message);
            continue;
        }
        if input == "history" {
            for (i, (act, _)) in states.iter().enumerate() {
                if i % 2 == 0 {
                    writeln!(current_message, "{:>2}. {}", 1 + (i >> 1), act).unwrap();
                } else {
                    writeln!(current_message, "    {}", act).unwrap();
                }
            }
            continue;
        }
        if input == "restart" {
            states.clear();
            current_player = Player::Player1;
            current_message.push_str("New Game");
            continue;
        }
        match cur.parse_move_for_player(current_player, input) {
            Err(msg) => {
                write!(current_message, "I don't recognize that: {}", msg).unwrap();
            }
            Ok(act) => {
                eprintln!("Action parsed: {:?}", act);
                match cur.perform_action(current_player, act) {
                    Err(e) => write!(current_message, "That move was not valid: {}", match e {
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
                    .unwrap(),
                    Ok(new_gs) => {
                        write!(current_message, "Last action by {}: {}", current_player, act)
                            .unwrap();
                        current_player = current_player.other();
                        states.push((act, new_gs));
                    }
                }
            }
        }
    }
}
