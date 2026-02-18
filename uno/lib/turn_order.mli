open Card

type player = {
  name : string;
  hand : card list;
}
(** A player in the UNO game. *)

(** The direction of play. *)
type direction =
  | Clockwise
  | Counterclockwise

(**
  {Abstraction Function}
  A [game_state] represents the current status of an UNO game, including:
    - the players in seating order,
    - which player's turn it is,
    - and the current direction of play (clockwise or counterclockwise).

  {Representation Invariant}
  - [players] is non-empty.
  - [0 <= current_index < List.length players].
  - Players appear in the list in their fixed turn order.
  - No duplicate player names.
*)

type game_state = {
  players : player list;
  current_index : int;
  direction : direction;
}

val next_index : game_state -> int
(** Inputs the game state and returns the index of the next player. *)

val skip_player : game_state -> game_state
(** Inputs the game state, skips one player, and returns the updated state. *)

val reverse_direction : game_state -> game_state
(** Reverses the direction of play and returns the updated game state. *)

val apply_card_effect : game_state -> card -> game_state
(** Inputs the game state and a card, and applies the effect of a played card
    and returns the updated game state. *)
