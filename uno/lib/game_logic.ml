open Card
open Turn_order

type table = {
  mutable deck : Deck.t;
  mutable discard : card list; (* head is top *)
  mutable current_color : string; (* matters after wild *)
}

(** [top_card tbl] returns the current top-of-discard card. Raises
    [Failure "no discard/top card"] if the discard pile is empty. *)
let top_card tbl =
  match tbl.discard with
  | [] -> failwith "no discard/top card"
  | c :: _ -> c

(** [set_top tbl c] pushes [c] onto the table's discard pile, making it the
    current top card. This mutates [tbl.discard]. *)
let set_top tbl c = tbl.discard <- c :: tbl.discard

(* Legal play: color match OR value/special match OR wild *)

(** [matches ~current_color top c] returns [true] if card [c] is a legal play on
    top-of-discard [top] under the given [current_color]. A wild or wild+4 is
    always playable. Otherwise the play is legal if the color matches
    [current_color], or the value or special type matches the top card. *)
let matches ~current_color (top : card) (c : card) =
  match c.special with
  | Some Wild | Some WildDraw4 -> true
  | _ -> (
      c.color = current_color
      ||
      match (top.special, c.special) with
      | None, None -> c.value = top.value
      | Some a, Some b -> a = b
      | _ -> false)

(* Remove card from hand when played *)

(** [remove_nth i xs] removes and returns the element at index [i] from list
    [xs], returning a pair [(picked, rest)]. Raises [Failure "bad index"] if [i]
    is out of bounds. This is used to remove a played card from a player's hand.
*)
let remove_nth i xs =
  let rec go k = function
    | [] -> failwith "bad index"
    | x :: xs when k = i -> (x, xs)
    | x :: xs ->
        let picked, rest = go (k + 1) xs in
        (picked, x :: rest)
  in
  go 0 xs

(* Draw n cards to player at index i *)

(** [give_cards_to state i n tbl] draws [n] cards from [tbl.deck] and adds them
    to player at index [i] in [state.players]. Returns the updated [game_state]
    (players updated immutably) and mutates [tbl.deck] to reflect the removed
    cards. *)
let give_cards_to (st : game_state) (i : int) n (tbl : table) : game_state =
  let cards, deck' = Deck.draw_n n tbl.deck in
  tbl.deck <- deck';
  let players' =
    List.mapi
      (fun j p -> if j = i then { p with hand = p.hand @ cards } else p)
      st.players
  in
  { st with players = players' }

(* Advance with effects; also handle Draw2/+4 *)

(** [apply_effects_and_advance st played tbl] applies the effect of [played] to
    [st] (for example Skip, Reverse, Draw2, Wild+4), updates the table as needed
    (cards drawn are taken from [tbl.deck]) and returns the resulting
    [game_state] where the turn has advanced according to the effect semantics.
*)
let apply_effects_and_advance (st : game_state) (played : card) (tbl : table) =
  let victim = next_index st in
  match played.special with
  | Some Draw2 ->
      let st' = skip_player st in
      give_cards_to st' victim 2 tbl
  | Some WildDraw4 ->
      let st' = skip_player st in
      give_cards_to st' victim 4 tbl
  | Some Reverse ->
      if List.length st.players = 2 then
        let st' = skip_player st in
        st'
      else apply_card_effect st played
  | Some Skip | Some Wild | None -> apply_card_effect st played

(* Ensures the the top card doesn't start on Wild/+4 for simplicity *)

(** [ensure_initial_top tbl] initializes the discard pile by drawing from
    [tbl.deck] until a non-wild card is found; it places that card on
    [tbl.discard] and sets [tbl.current_color] accordingly. If the deck is
    exhausted, it raises [Failure "deck exhausted at start"]. *)
let rec ensure_initial_top tbl =
  match Deck.draw_one tbl.deck with
  | None -> failwith "deck exhausted at start"
  | Some (c, d) -> (
      tbl.deck <- d;
      match c.special with
      | Some Wild | Some WildDraw4 -> ensure_initial_top tbl
      | _ ->
          tbl.discard <- [ c ];
          tbl.current_color <- c.color)

(** [deal_starting_hands st tbl] deals 7 cards to each player in [st] in
    round-robin order using [tbl.deck] as the draw pile. Returns the new
    [game_state] (players updated) and mutates [tbl.deck] to reflect the removed
    cards. *)
let deal_starting_hands (st : game_state) (tbl : table) : game_state =
  let rec deal_to_i i st =
    if i = List.length st.players then st
    else
      let cards, deck' = Deck.draw_n 7 tbl.deck in
      tbl.deck <- deck';
      let players' =
        List.mapi
          (fun j p -> if j = i then { p with hand = cards } else p)
          st.players
      in
      deal_to_i (i + 1) { st with players = players' }
  in
  deal_to_i 0 st
