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

module Deck = struct
  (* A deck/stack is just a list of cards; head = top of stack *)
  type t = card list

  let colors = [ "red"; "blue"; "green"; "yellow" ]

  (* 19 number cards per color:
     - one 0
     - two of each 1..9 *)
  let number_cards_for_color (color : string) : card list =
    let zero = { color; value = 0; special = None } in
    let others =
      List.init 9 (fun i ->
        let v = i + 1 in
        [
          { color; value = v; special = None };
          { color; value = v; special = None };
        ])
      |> List.flatten
    in
    zero :: others

  (* 2 Skip, 2 Reverse, 2 Draw2 per color = 6 per color *)
  let action_cards_for_color (color : string) : card list =
    let mk s = { color; value = 0; special = Some s } in
    [
      mk Skip; mk Skip;
      mk Reverse; mk Reverse;
      mk Draw2; mk Draw2;
    ]

  (* 8 black cards: 4 Wild, 4 WildDraw4 *)
  let black_cards : card list =
    let mk s = { color = "black"; value = 0; special = Some s } in
    [
      mk Wild; mk Wild; mk Wild; mk Wild;
      mk WildDraw4; mk WildDraw4; mk WildDraw4; mk WildDraw4;
    ]

  (* Fisher-Yates shuffle algorithm *)
  let shuffle (deck : t) : t =
    let arr = Array.of_list deck in
    let n = Array.length arr in
    for i = n - 1 downto 1 do
      let j = Random.int (i + 1) in
      let temp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- temp
    done;
    Array.to_list arr

  (* Build a full 108-card deck, shuffled *)
  let new_deck () : t =
    Random.self_init ();
    let per_color =
      List.map
        (fun c -> number_cards_for_color c @ action_cards_for_color c)
        colors
      |> List.flatten
    in
    let deck = per_color @ black_cards in
    shuffle deck

  (* Draw one card: return (card, remaining_deck) or None if empty *)
  let draw_one (deck : t) : (card * t) option =
    match deck with
    | [] -> None
    | c :: rest -> Some (c, rest)

  (* Draw n cards; returns (drawn_cards, remaining_deck).
     If there are fewer than n cards, it just returns as many as possible. *)
  let rec draw_n (n : int) (deck : t) : (card list * t) =
    if n <= 0 then ([], deck)
    else
      match draw_one deck with
      | None -> ([], deck)
      | Some (c, deck') ->
          let (cs, deck'') = draw_n (n - 1) deck' in
          (c :: cs, deck'')

  (* Helper: size of deck *)
  let size (deck : t) = List.length deck
end