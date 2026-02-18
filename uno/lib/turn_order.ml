open Card

(**A player represents one UNO player, with a name and a hand of cards.*)

type player = {
  name : string;
  hand : card list;
}

(**The direction of play represents the order in which players take their turns.*)
type direction =
  | Clockwise
  | Counterclockwise

type game_state = {
  players : player list;
  current_index : int;
  direction : direction;
}

(** Obtains the index of the next player by pattern matching against the
    direction of the state and incrementing accordingly.*)
let next_index state =
  let n = List.length state.players in
  match state.direction with
  | Clockwise -> (state.current_index + 1) mod n
  | Counterclockwise -> (state.current_index - 1 + n) mod n

(** Reverses the direction of the game state by pattern matching against the
    state direction and switching it, and creating a new state record with the
    updated direction.*)
let reverse_direction state =
  let new_dir =
    match state.direction with
    | Clockwise -> Counterclockwise
    | Counterclockwise -> Clockwise
  in
  { state with direction = new_dir }

(**Skips the next player by obtaining the index of who would have been next
   player and incrementing that index by 1 by pattern matching against state
   direction. Creates a new record with the new game state with the updated
   current index.*)
let skip_player state =
  let skipped_player_index = next_index state in
  let n = List.length state.players in
  let next_player_index =
    match state.direction with
    | Clockwise -> (skipped_player_index + 1) mod n
    | Counterclockwise -> (skipped_player_index - 1 + n) mod n
  in
  { state with current_index = next_player_index }

(**Applies the effect of each card by pattern matching against special cards and
   skipping turns or reversing direction, and updating the index of the next
   player by creating a new record as necessary.*)
let apply_card_effect state card =
  match card.special with
  | Some Skip -> skip_player state
  | Some Reverse ->
      let reversed_state = reverse_direction state in
      { reversed_state with current_index = next_index reversed_state }
  | Some Draw2 -> skip_player state
  | Some Wild -> { state with current_index = next_index state }
  | Some WildDraw4 -> skip_player state
  | None -> { state with current_index = next_index state }
