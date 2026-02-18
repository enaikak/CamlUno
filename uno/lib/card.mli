(**
  {Abstraction Function}
  A [card] represents a single UNO card. Its abstract meaning is determined by:
    - its [color], which indicates red/green/blue/yellow for normal or action
      cards, or black for wild cards;
    - its [value], which is the printed number for a number card (0–9);
    - its [special] field, which identifies whether the card has an action
      effect (Skip, Reverse, Draw2, Wild, WildDraw4).

  {Representation Invariant}
  - If [special = None], then [value] must be in the range 0–9, and [color] must
    be one of "red", "green", "blue", or "yellow".
  - If [special = Some Skip | Reverse | Draw2], then [color] must be one of the
    four normal colors and [value] must be 0.
  - If [special = Some Wild | WildDraw4], then [color] must be "black" and
    [value] must be 0.
*)

type special =
  | Skip
  | Reverse
  | Draw2
  | Wild
  | WildDraw4

type card = {
  color : string;
  value : int;
  special : special option;
}

(**
  {Abstraction Function}
  A [Deck.t] represents an UNO deck or draw pile. The head of the list is the
  top of the deck, meaning it is the next card to be drawn.

  {Representation Invariant}
  - The deck contains zero or more cards.
  - No card appears more times than in a real UNO deck.
  - The order of the list corresponds to the draw order: the first element is
    drawn first.
*)

module Deck : sig
  type t = card list

  val new_deck : unit -> t
  val shuffle : t -> t
  val draw_one : t -> (card * t) option
  val draw_n : int -> t -> card list * t
  val size : t -> int
end
