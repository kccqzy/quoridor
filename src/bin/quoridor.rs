use quoridor::Action;
use quoridor::FenceOrientation;
use quoridor::GameState;
use quoridor::Player;
use std::io::Write;

fn main() {
    let mut gs = GameState::new();
    std::io::stdout().lock().write_all(gs.draw().as_bytes()).unwrap();
    let actions = [
        Action::Move((7, 4)),
        Action::Move((1, 4)),
        Action::Move((6, 4)),
        Action::Move((2, 4)),
        Action::Move((5, 4)),
        Action::Move((3, 4)),
        Action::Fence((3, 3, FenceOrientation::Horizontal)),
        Action::Move((3, 3)),
    ];
    for (i, a) in actions.iter().enumerate() {
        let player = if i % 2 == 0 { Player::Player1 } else { Player::Player2 };
        match gs.perform_action(player, *a) {
            None => {
                eprintln!("invalid action");
                break;
            }
            Some(new_gs) => {
                gs = new_gs;
                std::io::stdout().lock().write_all(gs.draw().as_bytes()).unwrap();
            }
        }
    }
}
