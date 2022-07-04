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

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        if !input.is_ascii() {
            current_message = "not an ASCII string".into();
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
                current_message = "Cannot undo beyond the first action".into();
            } else {
                current_message = "Undo!".into();
                current_player = current_player.other();
                states.pop();
            }
            continue;
        }
        if input == "info" {
            current_message = cur.produce_info();
            continue;
        }
        if input == "history" {
            use std::fmt::Write;
            current_message.clear();
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
            current_message = "New Game".into();
            continue;
        }
        match cur.parse_move_for_player(current_player, input) {
            Err(msg) => {
                current_message = format!("I don't recognize that: {}", msg);
            }
            Ok(act) => {
                eprintln!("Action parsed: {:?}", act);
                match cur.perform_action(current_player, act) {
                    Err(msg) => {
                        current_message = format!("That move was not valid: {}", msg);
                    }
                    Ok(new_gs) => {
                        current_message = format!("Last action by {}: {}", current_player, act);
                        current_player = current_player.other();
                        states.push((act, new_gs));
                    }
                }
            }
        }
    }
}
