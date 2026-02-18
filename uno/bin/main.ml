open Uno.Card
open Uno.Turn_order
open Uno.Display_card
open Uno.Game_logic

(* Helper functions to print on terminal *)

let pp_card (c : card) = Uno.Display_card.colored_label c

let pp_hand hand =
  hand
  |> List.mapi (fun i c -> Printf.sprintf "%d) %s" (i + 1) (pp_card c))
  |> String.concat "\n"

(* Helper for checking number of card for play function *)
let is_number s =
  let n = String.length s in
  n > 0
  && String.for_all
       (function
         | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
         | _ -> false)
       s

(* Helper to read quit option to exit game *)
let read_line_trim () =
  try read_line () |> String.trim with End_of_file -> "quit"

(* Prompt player for chosen wild card color after played *)
let prompt_wild_color () =
  let rec loop () =
    print_string "Choose color [red|green|blue|yellow]: ";
    match String.lowercase_ascii (read_line_trim ()) with
    | ("red" | "green" | "blue" | "yellow") as c -> c
    | _ ->
        print_endline "Invalid color.";
        loop ()
  in
  loop ()

(** Clears the screen by printing new lines so previous player's hand is hidden
    from the next player.*)
let clear_screen () =
  for _ = 1 to 100 do
    print_endline ""
  done

let print_other_players_card_counts players current_index =
  print_endline "Other players cards:";
  List.iteri
    (fun i player ->
      if i <> current_index then
        let count = List.length player.hand in
        let word = if count = 1 then "card" else "cards" in
        Printf.printf "- %s: %d %s left\n" player.name count word)
    players;
  print_endline ""

let print_help () =
  print_endline "";
  print_endline
    "-----------------------------------------------------------------------------------------------------------------------------------------------------";
  print_endline "";
  print_endline
    "Objective: Be the first to get rid of all the cards in your hand";
  print_endline "";
  print_endline "Playing Cards:";
  print_endline "  You can play a card if it matches the discard pile by:";
  print_endline "    - Color (red, green, blue, yellow)";
  print_endline "    - Value (0–9)";
  print_endline "    - Special type (Skip, Reverse, Draw2)";
  print_endline "    - Wild or Wild +4 (which is always playable)";
  print_endline "  If you cannot play a card, you must draw from the draw pile.";
  print_endline "";
  print_endline "Card Effects:";
  print_endline "  Number: No effect; match color or value.";
  print_endline "  Skip: Next player loses their turn.";
  print_endline "  Reverse: Direction reverses.";
  print_endline "  Draw 2: Next player draws 2 cards and is skipped.";
  print_endline "  Wild: Choose the next color.";
  print_endline
    " Wild +4: Choose the next color; next player draws 4 and is skipped.";
  print_endline "";
  print_endline "Commands:";
  print_endline "  play <idx>   Play the card at position <idx>.";
  print_endline "  draw         Draw one card and end your turn.";
  print_endline "  help         Show this help menu.";
  print_endline "  quit         Leave the game.";
  print_endline "";
  print_endline "Troubleshooting:";
  print_endline
    "  - If an invalid index is chosen, the game will reject the move.";
  print_endline
    " - If an invalid command is written, you will be prompted again";
  print_endline
    "  - If you attempt to play an illegal card, you will be prompted again.";
  print_endline
    "------------------------------------------------------------------";
  print_endline "Press ENTER to return to the game.";
  print_endline "";
  ignore (read_line ())

(* Main loop for a player turn *)
let rec turn_loop (st : game_state) (tbl : table) =
  let me_i = st.current_index in
  let me = List.nth st.players me_i in
  clear_screen ();
  print_endline "Top of discard pile";
  print_endline "";
  let visual = render_card (top_card tbl) in
  List.iter print_endline visual;
  print_endline "";
  Printf.printf "Current color: %s\n" (color_word tbl.current_color);
  print_endline "";
  print_endline
    "-----------------------------------------------------------------------------------------------------------------------------------------------------";
  let n = List.length st.players in

  let prev_player_index =
    match st.direction with
    | Clockwise -> (st.current_index - 1 + n) mod n
    | Counterclockwise -> (st.current_index + 1) mod n
  in

  let next_player_index =
    match st.direction with
    | Clockwise -> (st.current_index + 1) mod n
    | Counterclockwise -> (st.current_index - 1 + n) mod n
  in

  let prev_player_name = (List.nth st.players prev_player_index).name in
  let next_player_name = (List.nth st.players next_player_index).name in

  print_endline "";
  Printf.printf "Current player: %s\n" me.name;
  print_endline "";
  Printf.printf "Previous player: %s\n" prev_player_name;
  Printf.printf "Next player: %s\n" next_player_name;
  print_endline "";
  print_other_players_card_counts st.players st.current_index;
  print_endline
    "-----------------------------------------------------------------------------------------------------------------------------------------------------";
  print_endline "";
  print_endline "Your hand:";
  print_endline "";
  print_endline (pp_hand me.hand);
  print_endline "";
  print_endline "Commands: play <idx> | draw | help | quit";

  print_string "> ";
  match String.split_on_char ' ' (read_line_trim ()) with
  | [ "quit" ] ->
      print_endline "Goodbye!";
      ()
  | [ "draw" ] ->
      let st' = give_cards_to st me_i 1 tbl in
      let st'' = { st' with current_index = next_index st' } in
      turn_loop st'' tbl
  | [ "help" ] ->
      print_help ();
      turn_loop st tbl
  | [ "play"; idx_s ] when is_number idx_s -> (
      let idx = int_of_string idx_s - 1 in
      match List.nth_opt me.hand idx with
      | None ->
          print_endline "Invalid index.";
          turn_loop st tbl
      | Some card ->
          if not (matches ~current_color:tbl.current_color (top_card tbl) card)
          then (
            print_endline "Illegal play.";
            turn_loop st tbl)
          else
            (* Remove from hand *)
            let played, new_hand = remove_nth idx me.hand in
            (* If wild, ask color before effects *)
            (match played.special with
            | Some Wild | Some WildDraw4 ->
                let chosen = prompt_wild_color () in
                tbl.current_color <- chosen
            | _ ->
                (* after playing a normal card, current color becomes the card's
                   color *)
                tbl.current_color <- played.color);
            set_top tbl played;
            (* Commit my hand *)
            let players' =
              List.mapi
                (fun j p -> if j = me_i then { p with hand = new_hand } else p)
                st.players
            in
            let st_committed = { st with players = players' } in

            (* Apply card effects + advance turn *)
            let st_after = apply_effects_and_advance st_committed played tbl in

            (* Win check *)
            let me_after = List.nth st_after.players me_i in
            if me_after.hand = [] then (
              Printf.printf "\nWinner: %s 🎉\n" me_after.name;
              ())
            else
              (* If not wild/+4 we already set current_color to played.color
                 above. For wild/+4 we already set chosen color. *)
              turn_loop st_after tbl)
  | _ ->
      print_endline "Unknown command.";
      turn_loop st tbl

(* Functions to assist in starting game *)

let rec read_players acc =
  Printf.printf "Enter player name (empty to start): ";
  match read_line_trim () with
  | "" ->
      let players = List.rev acc in
      if List.length players >= 2 && List.length players <= 4 then players
      else (
        print_endline "Need 2 to 4 players.";
        read_players acc)
  | name ->
      let p = { name; hand = [] } in
      read_players (p :: acc)

(* Main function to set everything up *)
let () =
  Random.self_init ();
  print_endline "Welcome to Uno!";
  let deck = Deck.new_deck () in
  let tbl = { deck; discard = []; current_color = "red" } in
  let players = read_players [] in
  let st0 = { players; current_index = 0; direction = Clockwise } in
  let st1 = deal_starting_hands st0 tbl in
  ensure_initial_top tbl;
  turn_loop st1 tbl
