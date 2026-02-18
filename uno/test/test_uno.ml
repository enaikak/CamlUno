open OUnit2
open Uno.Card
open Uno.Turn_order
open Uno.Display_card
open Uno.Game_logic

(* ===== Card and Deck Tests ===== *)

let test_deck_size _ =
  let deck = Deck.new_deck () in
  assert_equal 108 (Deck.size deck) ~msg:"Deck should have 108 cards"

let test_deck_has_all_colors _ =
  let deck = Deck.new_deck () in
  let has_red = List.exists (fun c -> c.color = "red") deck in
  let has_blue = List.exists (fun c -> c.color = "blue") deck in
  let has_green = List.exists (fun c -> c.color = "green") deck in
  let has_yellow = List.exists (fun c -> c.color = "yellow") deck in
  let has_black = List.exists (fun c -> c.color = "black") deck in
  assert_bool "Deck should have red cards" has_red;
  assert_bool "Deck should have blue cards" has_blue;
  assert_bool "Deck should have green cards" has_green;
  assert_bool "Deck should have yellow cards" has_yellow;
  assert_bool "Deck should have black cards" has_black

let test_deck_has_special_cards _ =
  let deck = Deck.new_deck () in
  let has_skip = List.exists (fun c -> c.special = Some Skip) deck in
  let has_reverse = List.exists (fun c -> c.special = Some Reverse) deck in
  let has_draw2 = List.exists (fun c -> c.special = Some Draw2) deck in
  let has_wild = List.exists (fun c -> c.special = Some Wild) deck in
  let has_wild_draw4 = List.exists (fun c -> c.special = Some WildDraw4) deck in
  assert_bool "Deck should have Skip cards" has_skip;
  assert_bool "Deck should have Reverse cards" has_reverse;
  assert_bool "Deck should have Draw2 cards" has_draw2;
  assert_bool "Deck should have Wild cards" has_wild;
  assert_bool "Deck should have WildDraw4 cards" has_wild_draw4

let test_draw_one _ =
  let deck = Deck.new_deck () in
  match Deck.draw_one deck with
  | Some (card, rest) ->
      assert_equal 107 (Deck.size rest)
        ~msg:"Remaining deck should have 107 cards";
      assert_bool "Card should have a color" (card.color <> "")
  | None -> assert_failure "Should be able to draw from a full deck"

let test_draw_one_empty _ =
  let empty_deck = [] in
  match Deck.draw_one empty_deck with
  | None -> assert_bool "Drawing from empty deck returns None" true
  | Some _ -> assert_failure "Should not be able to draw from empty deck"

let test_draw_n _ =
  let deck = Deck.new_deck () in
  let drawn, rest = Deck.draw_n 7 deck in
  assert_equal 7 (List.length drawn) ~msg:"Should draw 7 cards";
  assert_equal 101 (Deck.size rest) ~msg:"Remaining deck should have 101 cards"

let test_draw_n_more_than_available _ =
  let deck = Deck.new_deck () in
  let drawn, rest = Deck.draw_n 200 deck in
  assert_equal 108 (List.length drawn) ~msg:"Should draw all 108 cards";
  assert_equal 0 (Deck.size rest) ~msg:"Deck should be empty"

let test_deck_is_shuffled _ =
  let deck1 = Deck.new_deck () in
  let deck2 = Deck.new_deck () in
  (* With 108! possible orderings, two randomly shuffled decks should be
     different *)
  let same =
    List.for_all2
      (fun c1 c2 ->
        c1.color = c2.color && c1.value = c2.value && c1.special = c2.special)
      deck1 deck2
  in
  assert_bool "Two new decks should be shuffled differently (very likely)"
    (not same)

let make_test_game n_players =
  let players =
    List.init n_players (fun i ->
        { name = "Player" ^ string_of_int i; hand = [] })
  in
  { players; current_index = 0; direction = Clockwise }

let test_next_index_clockwise _ =
  let state = make_test_game 4 in
  assert_equal 1 (next_index state) ~msg:"Next index from 0 should be 1";
  let state2 = { state with current_index = 3 } in
  assert_equal 0 (next_index state2) ~msg:"Next index from 3 should wrap to 0"

let test_next_index_counterclockwise _ =
  let state = { (make_test_game 4) with direction = Counterclockwise } in
  assert_equal 3 (next_index state)
    ~msg:"Previous index from 0 should wrap to 3";
  let state2 = { state with current_index = 1 } in
  assert_equal 0 (next_index state2) ~msg:"Previous index from 1 should be 0"

let test_reverse_direction _ =
  let state = make_test_game 4 in
  let reversed = reverse_direction state in
  assert_equal Counterclockwise reversed.direction
    ~msg:"Should reverse to Counterclockwise";
  let reversed_again = reverse_direction reversed in
  assert_equal Clockwise reversed_again.direction
    ~msg:"Should reverse back to Clockwise"

let test_skip_player_clockwise _ =
  let state = make_test_game 4 in
  let skipped = skip_player state in
  assert_equal 2 skipped.current_index
    ~msg:"Should skip player 1, land on player 2"

let test_skip_player_counterclockwise _ =
  let state =
    { (make_test_game 4) with direction = Counterclockwise; current_index = 2 }
  in
  let skipped = skip_player state in
  assert_equal 0 skipped.current_index
    ~msg:"Should skip player 1, land on player 0"

let test_apply_skip_card _ =
  let state = make_test_game 4 in
  let skip_card = { color = "red"; value = 0; special = Some Skip } in
  let new_state = apply_card_effect state skip_card in
  assert_equal 2 new_state.current_index
    ~msg:"Skip card should skip next player"

let test_apply_reverse_card _ =
  let state = make_test_game 4 in
  let reverse_card = { color = "blue"; value = 0; special = Some Reverse } in
  let new_state = apply_card_effect state reverse_card in
  assert_equal Counterclockwise new_state.direction
    ~msg:"Reverse card should change direction";
  assert_equal 3 new_state.current_index
    ~msg:"After reverse, should move in new direction"

let test_apply_number_card _ =
  let state = make_test_game 4 in
  let number_card = { color = "green"; value = 5; special = None } in
  let new_state = apply_card_effect state number_card in
  assert_equal Clockwise new_state.direction ~msg:"Direction should not change";
  assert_equal 1 new_state.current_index ~msg:"Should advance to next player"

let test_apply_wild_card _ =
  let state = make_test_game 4 in
  let wild_card = { color = "black"; value = 0; special = Some Wild } in
  let new_state = apply_card_effect state wild_card in
  assert_equal 1 new_state.current_index
    ~msg:"Wild card should advance to next player"

let test_apply_wild_draw4_card _ =
  let state = make_test_game 4 in
  let wild_draw4 = { color = "black"; value = 0; special = Some WildDraw4 } in
  let new_state = apply_card_effect state wild_draw4 in
  assert_equal 2 new_state.current_index
    ~msg:"WildDraw4 should skip next player"

let test_render_card_basic _ =
  let card = { color = "red"; value = 5; special = None } in
  let lines = Uno.Display_card.render_card card in
  (* Check it's the expected size of 9 lines *)
  assert_equal 9 (List.length lines) ~msg:"Rendered card should have 9 lines";

  (* Check something simple: that the value '5' appears in at least one line *)
  assert_bool "Rendered card should contain the number 5"
    (List.exists (fun l -> String.contains l '5') lines)

let contains_sub (s : string) (sub : string) : bool =
  let sl = String.length s in
  let subl = String.length sub in
  if subl = 0 then true
  else
    let rec loop i =
      if i + subl > sl then false
      else if String.sub s i subl = sub then true
      else loop (i + 1)
    in
    loop 0

let test_colored_label_has_ansi _ =
  let card = { color = "red"; value = 7; special = None } in
  let label = Uno.Display_card.colored_label card in
  (* Expect a foreground red code and the reset code present *)
  let red_code = "\027[31m" in
  let reset_code = "\027[0m" in
  assert_bool "colored label contains red ANSI code"
    (contains_sub label red_code);
  assert_bool "colored label contains reset ANSI code"
    (contains_sub label reset_code)

let test_colored_label_green _ =
  let card = { color = "green"; value = 2; special = None } in
  let label = Uno.Display_card.colored_label card in
  let green_code = "\027[32m" in
  let reset_code = "\027[0m" in
  assert_bool "colored label contains green ANSI code"
    (contains_sub label green_code);
  assert_bool "colored label contains reset ANSI code"
    (contains_sub label reset_code)

let test_colored_label_blue _ =
  let card = { color = "blue"; value = 4; special = None } in
  let label = Uno.Display_card.colored_label card in
  let blue_code = "\027[34m" in
  let reset_code = "\027[0m" in
  assert_bool "colored label contains blue ANSI code"
    (contains_sub label blue_code);
  assert_bool "colored label contains reset ANSI code"
    (contains_sub label reset_code)

let test_colored_label_yellow _ =
  let card = { color = "yellow"; value = 9; special = None } in
  let label = Uno.Display_card.colored_label card in
  let yellow_code = "\027[33m" in
  let reset_code = "\027[0m" in
  assert_bool "colored label contains yellow ANSI code"
    (contains_sub label yellow_code);
  assert_bool "colored label contains reset ANSI code"
    (contains_sub label reset_code)

let test_render_card_contains_color_code _ =
  let card = { color = "blue"; value = 8; special = None } in
  let lines = Uno.Display_card.render_card card in
  let bg_blue = "\027[44m" in
  assert_bool "Rendered card contains blue background ANSI code"
    (List.exists (fun l -> contains_sub l bg_blue) lines)

let test_render_wild_contains_bg _ =
  let card = { color = "black"; value = 0; special = Some Wild } in
  let lines = Uno.Display_card.render_card card in
  let bg_red = "\027[41m" in
  let bg_green = "\027[42m" in
  let bg_blue = "\027[44m" in
  let bg_yellow = "\027[43m" in
  let has_any_bg =
    List.exists
      (fun l ->
        contains_sub l bg_red || contains_sub l bg_green
        || contains_sub l bg_blue || contains_sub l bg_yellow)
      lines
  in
  assert_bool "Wild rendered card contains at least one bg color code"
    has_any_bg

let test_render_card_specials _ =
  let skip = { color = "red"; value = 0; special = Some Skip } in
  let rev = { color = "blue"; value = 0; special = Some Reverse } in
  let d2 = { color = "green"; value = 0; special = Some Draw2 } in
  let wild = { color = "black"; value = 0; special = Some Wild } in
  let w4 = { color = "black"; value = 0; special = Some WildDraw4 } in

  let lskip = Uno.Display_card.render_card skip in
  assert_bool "skip shows X in corner"
    (List.exists (fun s -> String.contains s 'X') lskip);

  let lrev = Uno.Display_card.render_card rev in
  assert_bool "reverse shows R in corner"
    (List.exists (fun s -> String.contains s 'R') lrev);

  let ld2 = Uno.Display_card.render_card d2 in
  assert_bool "draw2 shows +2 in corner"
    (List.exists (fun s -> contains_sub s "+2") ld2);

  let lwild = Uno.Display_card.render_card wild in
  assert_bool "wild includes bg color"
    (List.exists
       (fun s ->
         contains_sub s "\027[41m" || contains_sub s "\027[42m"
         || contains_sub s "\027[44m" || contains_sub s "\027[43m")
       lwild);

  let lw4 = Uno.Display_card.render_card w4 in
  assert_bool "wild+4 contains +4 in center or corner"
    (List.exists (fun s -> contains_sub s "+4") lw4)

let test_colored_label_skip _ =
  let card = { color = "red"; value = 0; special = Some Skip } in
  let label = Uno.Display_card.colored_label card in
  assert_bool "skip label contains SKIP" (contains_sub label "SKIP");
  assert_bool "skip label contains reset" (contains_sub label "\027[0m")

let test_colored_label_reverse _ =
  let card = { color = "blue"; value = 0; special = Some Reverse } in
  let label = Uno.Display_card.colored_label card in
  assert_bool "reverse label contains REVERSE" (contains_sub label "REVERSE");
  assert_bool "reverse label contains reset" (contains_sub label "\027[0m")

let test_colored_label_draw2 _ =
  let card = { color = "green"; value = 0; special = Some Draw2 } in
  let label = Uno.Display_card.colored_label card in
  assert_bool "draw2 label contains DRAW 2" (contains_sub label "DRAW 2");
  assert_bool "draw2 label contains reset" (contains_sub label "\027[0m")

let test_colored_label_wild _ =
  let card = { color = "black"; value = 0; special = Some Wild } in
  let label = Uno.Display_card.colored_label card in
  assert_bool "wild label contains colored W" (contains_sub label "\027[31mW");
  assert_bool "wild label contains colored I" (contains_sub label "\027[33mI");
  assert_bool "wild label contains colored L" (contains_sub label "\027[32mL");
  assert_bool "wild label contains colored D" (contains_sub label "\027[34mD");

  assert_bool "wild label contains reset" (contains_sub label "\027[0m")

let test_colored_label_wilddraw4 _ =
  let card = { color = "black"; value = 0; special = Some WildDraw4 } in
  let label = Uno.Display_card.colored_label card in
  assert_bool "wild+4 label contains colored W" (contains_sub label "\027[31mW");
  assert_bool "wild+4 label contains colored I" (contains_sub label "\027[33mI");
  assert_bool "wild+4 label contains colored L" (contains_sub label "\027[32mL");
  assert_bool "wild+4 label contains colored D" (contains_sub label "\027[34mD");
  assert_bool "wild+4 label contains +4" (contains_sub label "+4");
  assert_bool "wild+4 label contains reset" (contains_sub label "\027[0m")

let test_top_card_empty_raises _ =
  let tbl = { deck = []; discard = []; current_color = "red" } in
  assert_raises (Failure "no discard/top card") (fun () ->
      ignore (top_card tbl))

let test_set_top_and_top_card _ =
  let c = { color = "red"; value = 3; special = None } in
  let tbl = { deck = []; discard = []; current_color = "red" } in
  set_top tbl c;
  assert_equal c (top_card tbl)

let test_matches_mismatched_special_returns_false _ =
  let top = { color = "red"; value = 5; special = None } in
  let c = { color = "blue"; value = 1; special = Some Skip } in
  assert_bool "mismatch should be false"
    (not (matches ~current_color:"green" top c))

let test_remove_nth_bad_index_raises _ =
  let hand = [ { color = "red"; value = 1; special = None } ] in
  assert_raises (Failure "bad index") (fun () -> ignore (remove_nth 5 hand))

let test_apply_effects_wild_draw4 _ =
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  let st = make_test_game 4 in
  let w4 = { color = "black"; value = 0; special = Some WildDraw4 } in
  let st_after = apply_effects_and_advance st w4 tbl in
  let victim = List.nth st_after.players (next_index st) in
  assert_equal 4 (List.length victim.hand)

let test_apply_effects_reverse_two_players _ =
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  let st = make_test_game 2 in
  let rev = { color = "yellow"; value = 0; special = Some Reverse } in
  let st_after = apply_effects_and_advance st rev tbl in
  (* For 2 players Reverse acts like skip: current_index should remain 0 and
     direction unchanged *)
  assert_equal Clockwise st_after.direction;
  assert_equal 0 st_after.current_index

let test_ensure_initial_top_empty_deck_raises _ =
  let tbl = { deck = []; discard = []; current_color = "red" } in
  assert_raises (Failure "deck exhausted at start") (fun () ->
      ensure_initial_top tbl)

let test_ensure_initial_top_skips_initial_wild _ =
  let wild = { color = "black"; value = 0; special = Some Wild } in
  let normal = { color = "red"; value = 3; special = None } in
  let tbl = { deck = [ wild; normal ]; discard = []; current_color = "red" } in
  ensure_initial_top tbl;
  assert_equal normal (top_card tbl);
  assert_equal "red" tbl.current_color

let test_matches_same_color _ =
  let top = { color = "red"; value = 6; special = None } in
  let card = { color = "red"; value = 7; special = None } in
  assert_bool "Cards with the same color should match"
    (matches ~current_color:"red" top card)

let test_matches_same_value _ =
  let top = { color = "red"; value = 5; special = None } in
  let card = { color = "blue"; value = 5; special = None } in
  assert_bool "Cards with the same value should match"
    (matches ~current_color:"red" top card)

let test_matches_same_special _ =
  let top = { color = "green"; value = 0; special = Some Skip } in
  let card = { color = "yellow"; value = 0; special = Some Skip } in
  assert_bool "Cards with same the special type should match"
    (matches ~current_color:"green" top card)

let test_matches_wild _ =
  let top = { color = "red"; value = 5; special = None } in
  let wild = { color = "black"; value = 0; special = Some Wild } in
  assert_bool "Wild card should always match"
    (matches ~current_color:"blue" top wild)

let test_remove_nth_middle _ =
  let hand =
    [
      { color = "red"; value = 6; special = None };
      { color = "blue"; value = 7; special = None };
      { color = "yellow"; value = 3; special = None };
    ]
  in
  let picked, rest = remove_nth 1 hand in
  assert_equal 7 picked.value ~msg:"Should remove the middle card";
  assert_equal 2 (List.length rest)
    ~msg:"List should have 2 cards after removal"

let test_remove_nth_first _ =
  let hand =
    [
      { color = "red"; value = 6; special = None };
      { color = "blue"; value = 7; special = None };
    ]
  in
  let picked, rest = remove_nth 0 hand in
  assert_equal 6 picked.value ~msg:"Should remove the first card";
  assert_equal 1 (List.length rest) ~msg:"List should have 1 card left"

let test_give_cards_to _ =
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  let st =
    {
      players =
        [
          { name = "A"; hand = [] };
          { name = "B"; hand = [] };
          { name = "C"; hand = [] };
        ];
      current_index = 0;
      direction = Clockwise;
    }
  in
  let st_after = give_cards_to st 1 3 tbl in
  assert_equal 3
    (List.length (List.nth st_after.players 1).hand)
    ~msg:"Player 1 should draw 3 cards"

let test_apply_effects_draw2 _ =
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  let st = make_test_game 4 in
  let draw2 = { color = "blue"; value = 0; special = Some Draw2 } in
  let st_after = apply_effects_and_advance st draw2 tbl in
  let victim = List.nth st_after.players 1 in
  assert_equal 2 (List.length victim.hand)
    ~msg:"Draw2 should force the next player to draw 2 cards"

let test_apply_effects_reverse _ =
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  let st = make_test_game 4 in
  let rev = { color = "yellow"; value = 0; special = Some Reverse } in
  let st_after = apply_effects_and_advance st rev tbl in
  assert_equal Counterclockwise st_after.direction
    ~msg:"Reverse should flip the direction"

let test_apply_effects_skip _ =
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  let st = make_test_game 4 in
  let skip = { color = "green"; value = 0; special = Some Skip } in
  let st_after = apply_effects_and_advance st skip tbl in
  assert_equal 2 st_after.current_index ~msg:"Skip should advance 2 positions"

let test_ensure_initial_top_never_wild _ =
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  ensure_initial_top tbl;
  match tbl.discard with
  | [] -> assert_failure "Discard should not be empty after initializing"
  | c :: _ ->
      assert_bool "Initial top card must not be wild"
        (c.special <> Some Wild && c.special <> Some WildDraw4)

let test_deal_starting_hands _ =
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  let st = make_test_game 3 in
  let st_after = deal_starting_hands st tbl in
  List.iter
    (fun p ->
      assert_equal 7 (List.length p.hand)
        ~msg:"Each player should be dealt 7 cards")
    st_after.players

(* Strip ANSI escape codes so we can assert on the plain text content *)
let strip_ansi s =
  let buf = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else if s.[i] = '\027' then (
      (* Skip ESC ... m *)
      let j = ref (i + 1) in
      while !j < String.length s && s.[!j] <> 'm' do
        incr j
      done;
      let next = if !j < String.length s then !j + 1 else !j in
      loop next)
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

(* Plain-text checks for colored_label and color_word (used by main.ml) *)
let test_colored_label_number_plain_text _ =
  let card = { color = "red"; value = 7; special = None } in
  let plain = Uno.Display_card.colored_label card |> strip_ansi in
  assert_equal "red 7" plain
    ~msg:"colored_label for number card should be 'red 7' without ANSI"

let test_colored_label_skip_plain_text _ =
  let card = { color = "blue"; value = 0; special = Some Skip } in
  let plain = Uno.Display_card.colored_label card |> strip_ansi in
  assert_equal "blue SKIP" plain
    ~msg:"colored_label for Skip card should be 'blue SKIP' without ANSI"

let test_colored_label_draw2_plain_text _ =
  let card = { color = "yellow"; value = 0; special = Some Draw2 } in
  let plain = Uno.Display_card.colored_label card |> strip_ansi in
  assert_equal "yellow DRAW 2" plain
    ~msg:"colored_label for Draw2 card should be 'yellow DRAW 2' without ANSI"

let test_colored_label_wild_plain_text _ =
  let card = { color = "black"; value = 0; special = Some Wild } in
  let plain = Uno.Display_card.colored_label card |> strip_ansi in
  assert_equal "WILD" plain
    ~msg:"colored_label for Wild card should be 'WILD' without ANSI"

let test_colored_label_wild_draw4_plain_text _ =
  let card = { color = "black"; value = 0; special = Some WildDraw4 } in
  let plain = Uno.Display_card.colored_label card |> strip_ansi in
  assert_equal "WILD +4" plain
    ~msg:"colored_label for Wild +4 should be 'WILD +4' without ANSI"

let test_color_word_plain_text _ =
  let red_plain = Uno.Display_card.color_word "red" |> strip_ansi in
  let blue_plain = Uno.Display_card.color_word "blue" |> strip_ansi in
  assert_equal "RED" red_plain ~msg:"color_word 'red' should show 'RED'";
  assert_equal "BLUE" blue_plain ~msg:"color_word 'blue' should show 'BLUE'"

let test_matches_uses_current_color_not_top_color _ =
  (* Simulate a situation after a Wild: the top card might be black, but
     current_color has been set to (say) blue. *)
  let top = { color = "black"; value = 0; special = Some Wild } in
  let card = { color = "blue"; value = 9; special = None } in
  assert_bool
    "Card should be playable when its color matches current_color even if it \
     doesn't match top.color"
    (matches ~current_color:"blue" top card)

let test_color_word_has_ansi_codes _ =
  let s = Uno.Display_card.color_word "red" in
  let reset = "\027[0m" in
  assert_bool "color_word should contain some ANSI sequence"
    (contains_sub s "\027[");
  assert_bool "color_word for red should contain the reset code"
    (contains_sub s reset)

let test_matches_no_match _ =
  let top = { color = "red"; value = 5; special = None } in
  let card = { color = "green"; value = 7; special = Some Reverse } in
  assert_bool "Card with different color, value, and special should not match"
    (not (matches ~current_color:"blue" top card))

let test_matches_wild_draw4_always_matches _ =
  let top = { color = "yellow"; value = 3; special = None } in
  let w4 = { color = "black"; value = 0; special = Some WildDraw4 } in
  assert_bool "WildDraw4 should always be playable regardless of top card"
    (matches ~current_color:"green" top w4)

let card_tests =
  "Card and Deck Tests"
  >::: [
         "deck size" >:: test_deck_size;
         "deck has all colors" >:: test_deck_has_all_colors;
         "deck has special cards" >:: test_deck_has_special_cards;
         "draw one card" >:: test_draw_one;
         "draw one with empty deck" >:: test_draw_one_empty;
         "draw n cards" >:: test_draw_n;
         "draw n more than available cards" >:: test_draw_n_more_than_available;
         "deck is shuffled" >:: test_deck_is_shuffled;
       ]

let turn_order_tests =
  "Turn Order Tests"
  >::: [
         "get the next index for clockwise direction"
         >:: test_next_index_clockwise;
         "get the next index for counterclockwise direction"
         >:: test_next_index_counterclockwise;
         "reverse the direction" >:: test_reverse_direction;
         "skip the next player in clockwise direction"
         >:: test_skip_player_clockwise;
         "skip the next player in counterclockwise direction"
         >:: test_skip_player_counterclockwise;
         "apply a skip card" >:: test_apply_skip_card;
         "apply a reverse card" >:: test_apply_reverse_card;
         "apply a number card" >:: test_apply_number_card;
         "apply a wild card" >:: test_apply_wild_card;
         "apply a wild draw4 card" >:: test_apply_wild_draw4_card;
       ]

let display_tests =
  "Display Card Tests"
  >::: [
         "render_card_basic" >:: test_render_card_basic;
         "colored_label_has_ansi" >:: test_colored_label_has_ansi;
         "colored_label_green" >:: test_colored_label_green;
         "colored_label_blue" >:: test_colored_label_blue;
         "colored_label_yellow" >:: test_colored_label_yellow;
         "render_card_contains_color_code"
         >:: test_render_card_contains_color_code;
         "render_card_wild_bg" >:: test_render_wild_contains_bg;
         "render_card_specials" >:: test_render_card_specials;
         "colored_label_skip" >:: test_colored_label_skip;
         "colored_label_reverse" >:: test_colored_label_reverse;
         "colored_label_draw2" >:: test_colored_label_draw2;
         "colored_label_wild" >:: test_colored_label_wild;
         "colored_label_wilddraw4" >:: test_colored_label_wilddraw4;
       ]

let game_logic_tests =
  "Game Logic Tests"
  >::: [
         "matches the same color" >:: test_matches_same_color;
         "matches the same value" >:: test_matches_same_value;
         "matches the same special type" >:: test_matches_same_special;
         "matches a wild card" >:: test_matches_wild;
         "removes nth card from the middle of a hand" >:: test_remove_nth_middle;
         "remove nth card from first card in a hand" >:: test_remove_nth_first;
         "give cards to a player" >:: test_give_cards_to;
         "apply the effects of the draw2 card" >:: test_apply_effects_draw2;
         "apply the effects of the reverse card" >:: test_apply_effects_reverse;
         "apply the effects of the skip card" >:: test_apply_effects_skip;
         "top_card empty raises" >:: test_top_card_empty_raises;
         "set_top and top_card" >:: test_set_top_and_top_card;
         "matches mismatch special false"
         >:: test_matches_mismatched_special_returns_false;
         "remove nth bad index raises" >:: test_remove_nth_bad_index_raises;
         "apply effects wild draw4" >:: test_apply_effects_wild_draw4;
         "apply effects reverse two players"
         >:: test_apply_effects_reverse_two_players;
         "ensure initial top empty deck raises"
         >:: test_ensure_initial_top_empty_deck_raises;
         "ensure initial top skips wild"
         >:: test_ensure_initial_top_skips_initial_wild;
         "ensure the initial top of the discard pile is never a wild card"
         >:: test_ensure_initial_top_never_wild;
         "deal the starting hands to each player" >:: test_deal_starting_hands;
         "two cards with no matches returns false" >:: test_matches_no_match;
         "wild draw4 always matches" >:: test_matches_wild_draw4_always_matches;
       ]

let main_tests =
  "Main.ml Basic Functionality Tests"
  >::: [
         "card has proper color and number"
         >:: test_colored_label_number_plain_text;
         "test normal colored skip card" >:: test_colored_label_skip_plain_text;
         "test normal colored draw 2 card"
         >:: test_colored_label_draw2_plain_text;
         "test wild card" >:: test_colored_label_wild_plain_text;
         "test wild draw 4 card" >:: test_colored_label_wild_draw4_plain_text;
         "test color of the card when prompted" >:: test_color_word_plain_text;
         "" >:: test_matches_uses_current_color_not_top_color;
         "" >:: test_color_word_has_ansi_codes;
       ]

let tests =
  "UNO Game Test Suite"
  >::: [
         card_tests;
         turn_order_tests;
         display_tests;
         game_logic_tests;
         main_tests;
       ]

let _ = run_test_tt_main tests
