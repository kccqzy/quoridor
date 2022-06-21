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
    let mut gs = vec![GameState::new()];
    let mut current_player = Player::Player1;
    let mut current_message = "New Game".into();

    while !gs.last().unwrap().is_game_complete() {
        clear_screen().unwrap();
        std::io::stdout().lock().write_all(gs.last().unwrap().draw(true).as_bytes()).unwrap();
        println!("{}", current_message);
        println!("Current Player: {}", current_player);

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        match gs.last().unwrap().parse_move_for_player(current_player, input.trim()) {
            Err(msg) => {
                current_message = format!("I don't recognize that: {}", msg);
            }
            Ok(act) => {
                eprintln!("Action parsed: {:?}", act);
                match gs.last().unwrap().perform_action(current_player, act) {
                    Err(msg) => {
                        current_message = format!("That move was not valid: {}", msg);
                    }
                    Ok(new_gs) => {
                        current_message =
                            format!("Last action by {}: {}", current_player, input.trim());
                        current_player = current_player.other();
                        gs.push(new_gs);
                    }
                }
            }
        }
    }
}
