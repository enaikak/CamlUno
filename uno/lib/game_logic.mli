open Card
open Turn_order

type table = {
  mutable deck : Deck.t;
  mutable discard : card list;
  mutable current_color : string;
}
(** Game-level helpers and simple table state.

    Abstraction function:
    - A [table] represents the shared table state of an UNO game: the draw pile
      (deck), the discard pile (head is top) and the current active color
      (important after a wild play).

    Representation invariant:
    - [deck] is any valid [Deck.t] (list of cards).
    - [discard] is a list of cards where head is the most recent discard.
    - [current_color] is a lower-case string representing one of the normal
      colors ("red","green","blue","yellow"); tests and callers ensure
      reasonable values are supplied. *)

val top_card : table -> card
(** [top_card tbl] returns the current top card of [tbl.discard]. Raises
    [Failure "no discard/top card"] if the discard pile is empty. *)

val set_top : table -> card -> unit
(** [set_top tbl c] pushes card [c] onto the top of [tbl.discard]. *)

val matches : current_color:string -> card -> card -> bool
(** [matches ~current_color top c] is [true] if card [c] is a legal play on
    [top] given the active color [current_color]. Wild cards always match. *)

val remove_nth : int -> 'a list -> 'a * 'a list
(** [remove_nth i xs] removes and returns the element at index [i] in [xs].
    Raises [Failure "bad index"] if [i] is out of bounds. *)

val give_cards_to : game_state -> int -> int -> table -> game_state
(** [give_cards_to st i n tbl] draws [n] cards from [tbl.deck] and adds them to
    player index [i] in [st]. Updates both [st] and [tbl.deck]. *)

val apply_effects_and_advance : game_state -> card -> table -> game_state
(** [apply_effects_and_advance st played tbl] applies the special effect of
    [played] (Skip, Reverse, Draw2, WildDraw4), updates the table as needed, and
    returns the resulting game state with the turn advanced. *)

val ensure_initial_top : table -> unit
(** [ensure_initial_top tbl] draws cards from [tbl.deck] until a non-wild card
    is found and places it on the discard pile. Sets [tbl.current_color]. Raises
    [Failure "deck exhausted at start"] if no such card exists. *)

val deal_starting_hands : game_state -> table -> game_state
(** [deal_starting_hands st tbl] deals 7 cards to each player in [st] using
    cards drawn from [tbl.deck]. Returns the updated game state. *)
