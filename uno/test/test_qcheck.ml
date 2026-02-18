open QCheck
open Uno.Card

(* Simple equality for cards *)
let card_eq a b =
  a.color = b.color && a.value = b.value && a.special = b.special

(* Remove one occurrence of x from a list using equality eq. Return Some rest or None *)
let rec remove_one_eq eq x = function
  | [] -> None
  | y :: ys -> if eq x y then Some ys else Option.map (fun r -> y :: r) (remove_one_eq eq x ys)

let multisets_equal ~eq a b =
  let rec check xs ys =
    match xs with
    | [] -> true
    | x :: xs' -> (
        match remove_one_eq eq x ys with
        | None -> false
        | Some ys' -> check xs' ys')
  in
  check a b && check b a

(* Generators: small decks for fast checks *)
(* For testing purposes we pick cards from a fixed sample set to avoid complex
   generator plumbing while still exercising shuffle correctness. *)
let sample_cards =
  [ { color = "red"; value = 0; special = None }; { color = "red"; value = 7; special = None }
  ; { color = "blue"; value = 3; special = None }; { color = "green"; value = 5; special = None }
  ; { color = "yellow"; value = 9; special = None }
  ; { color = "red"; value = 0; special = Some Skip }
  ; { color = "blue"; value = 0; special = Some Reverse }
  ; { color = "green"; value = 0; special = Some Draw2 }
  ; { color = "black"; value = 0; special = Some Wild }
  ; { color = "black"; value = 0; special = Some WildDraw4 }
  ]

let gen_card = Gen.oneofl sample_cards
let gen_deck = Gen.list_size (Gen.int_range 0 20) gen_card
let arb_deck = make ~print:(fun _ -> "<deck>") gen_deck

(* Property: shuffle preserves multiset (permutation) *)
let prop_shuffle_preserves_multiset =
  Test.make ~name:"shuffle preserves multiset" ~count:200 arb_deck (fun deck ->
      let shuffled = Uno.Card.Deck.shuffle deck in
      multisets_equal ~eq:card_eq deck shuffled)

(* Property: new_deck produces 108 cards *)
let prop_new_deck_size =
  Test.make ~name:"new_deck size 108" ~count:50 (make Gen.unit) (fun () ->
      List.length (Uno.Card.Deck.new_deck ()) = 108)

let () = QCheck_runner.run_tests_main [ prop_shuffle_preserves_multiset; prop_new_deck_size ]
