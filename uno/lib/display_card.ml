open Card

(** Display helpers for terminal rendering of UNO cards. Functions in this
    module build a small ASCII-art card representation and optionally wrap parts
    of the card in ANSI color codes for terminal display. *)

(** ANSI reset sequence used to end colored output. *)
let reset = "\027[0m"

let fg_white = "\027[97m"
let fg_red = "\027[31m"
let fg_green = "\027[32m"
let fg_yellow = "\027[33m"
let fg_blue = "\027[34m"
let bg_red = "\027[41m"
let bg_green = "\027[42m"
let bg_yellow = "\027[43m"
let bg_blue = "\027[44m"
let bg_black = "\027[40m"

(** [bg_of_color color] returns an ANSI background color escape sequence
    corresponding to [color]. Accepted inputs are "red", "green", "yellow",
    "blue"; any other value maps to a black background. *)
let bg_of_color color =
  match String.lowercase_ascii color with
  | "red" -> bg_red
  | "green" -> bg_green
  | "yellow" -> bg_yellow
  | "blue" -> bg_blue
  | _ -> bg_black

(** [fg_of_color color] returns an ANSI foreground color escape sequence for the
    given [color]. Unknown colors map to black (no emphasis). *)
let fg_of_color color =
  match String.lowercase_ascii color with
  | "red" -> fg_red
  | "green" -> fg_green
  | "yellow" -> fg_yellow
  | "blue" -> fg_blue
  | _ -> ""

(** [center_text card] returns the string that should appear centered on the
    ASCII card for [card]. For number cards it is the number; for special cards
    it is the textual representation ("SKIP", "REVERSE", etc.). *)
let center_text card =
  match card.special with
  | None -> string_of_int card.value
  | Some Skip -> "SKIP"
  | Some Reverse -> "REVERSE"
  | Some Draw2 -> "DRAW 2"
  | Some Wild -> "WILD"
  | Some WildDraw4 -> "WILD +4"

(** [corner_text card] returns a short token suitable for display in a corner of
    the ASCII card (single-letter or short symbol). *)
let corner_text card =
  match card.special with
  | None -> string_of_int card.value
  | Some Skip -> "X"
  | Some Reverse -> "R"
  | Some Draw2 -> "+2"
  | Some Wild -> "W"
  | Some WildDraw4 -> "+4"

(** [card_outline ()] returns a skeletal ASCII card made of 9 lines. Tests and
    renderers fill in content at specific indices. *)
let card_outline () =
  let top = "+---------------+" in
  let middle = "|               |" in
  let bottom = "+---------------+" in
  [ top; middle; middle; middle; middle; middle; middle; middle; bottom ]

(** [replace_range line start replacement] replaces the segment of [line]
    starting at [start] with [replacement], assuming [replacement] has the same
    length as the replaced segment. Use with care: indices must be valid. *)
let replace_range line start replacement =
  let prefix = String.sub line 0 start in
  let suffix =
    String.sub line
      (start + String.length replacement)
      (String.length line - start - String.length replacement)
  in
  prefix ^ replacement ^ suffix

(** [replace_with_len line start orig_len replacement] replaces [orig_len]
    characters in [line] starting at [start] with [replacement]. It is used to
    insert colored substrings where the replacement length may differ from the
    original. *)
let replace_with_len line start orig_len replacement =
  let prefix = String.sub line 0 start in
  let suffix =
    String.sub line (start + orig_len) (String.length line - start - orig_len)
  in
  prefix ^ replacement ^ suffix

(** [square color] produces a single background-colored square token used when
    rendering wild cards (a small visual indicator). *)
let square color = bg_of_color color ^ " " ^ reset

let wild_card_squares =
  square "red" ^ " " ^ square "green" ^ " " ^ square "blue" ^ " "
  ^ square "yellow"

(** [fill_card outline card] fills the ASCII [outline] with the [card] content:
    corner tokens, centered text, and for wild cards a row of color squares.
    Returns the list of lines representing the filled card. *)
let fill_card outline card =
  let corner = corner_text card in
  let center = center_text card in
  let arr = Array.of_list outline in
  arr.(1) <- replace_range arr.(1) 2 corner;
  arr.(7) <- replace_range arr.(7) (15 - String.length corner) corner;
  let center_start = (17 - String.length center) / 2 in
  arr.(4) <- replace_range arr.(4) center_start center;

  (match card.special with
  | Some Wild | Some WildDraw4 ->
      arr.(5) <- "|    " ^ wild_card_squares ^ "    |"
  | _ -> ());

  Array.to_list arr

let build_card card = fill_card (card_outline ()) card

(** [color_substring line start len color_code] wraps the substring of [line]
    beginning at [start] with length [len] in [color_code] and the reset
    sequence. Returns the updated line. *)

let color_substring line start len color_code =
  let substr = String.sub line start len in
  let colored = color_code ^ substr in
  replace_with_len line start len colored

(** [color_card card lines] applies foreground coloring to the relevant portions
    (corner and center) of [lines] for non-wild cards. Wild cards are left
    unchanged because they use multi-colored background squares. *)
let color_card card lines =
  match card.special with
  | Some Wild | Some WildDraw4 -> lines
  | _ ->
      let corner = corner_text card in
      let center = center_text card in
      let corner_len = String.length corner in
      let center_len = String.length center in
      let center_start = (17 - center_len) / 2 in
      let fg = fg_white in
      List.mapi
        (fun idx line ->
          match idx with
          | 1 -> color_substring line 2 corner_len fg
          | 7 -> color_substring line (15 - corner_len) corner_len fg
          | 4 -> color_substring line center_start center_len fg
          | _ -> line)
        lines

let apply_background card lines =
  let bg = bg_of_color card.color in
  List.map (fun line -> bg ^ line ^ reset) lines

(** [render_card card] returns the ASCII-art representation of [card] as a list
    of lines with appropriate coloring applied. This is the primary function
    used by the UI to show a single card. *)
let render_card card =
  let base = build_card card in
  let colored_text = color_card card base in
  match card.special with
  | Some Wild | Some WildDraw4 -> colored_text
  | _ -> apply_background card colored_text

let color_word color = fg_of_color color ^ String.uppercase_ascii color ^ reset

let multicolor_wild =
  fg_red ^ "W" ^ reset ^ fg_yellow ^ "I" ^ reset ^ fg_green ^ "L" ^ reset
  ^ fg_blue ^ "D" ^ reset

(** [colored_label card] produces a one-line textual label for [card] where the
    value or special token is wrapped with an ANSI foreground color
    corresponding to the card color. Useful for compact hand displays. *)
let colored_label card =
  match card.special with
  | Some Wild -> multicolor_wild
  | Some WildDraw4 -> multicolor_wild ^ " " ^ fg_white ^ "+4" ^ reset
  | _ -> (
      let fg = fg_of_color card.color in
      let color_part = fg ^ card.color ^ reset in
      let white = fg_white in
      match card.special with
      | None -> color_part ^ " " ^ white ^ string_of_int card.value ^ reset
      | Some Skip -> color_part ^ " " ^ white ^ "SKIP" ^ reset
      | Some Reverse -> color_part ^ " " ^ white ^ "REVERSE" ^ reset
      | Some Draw2 -> color_part ^ " " ^ white ^ "DRAW 2" ^ reset
      | Some _ -> "")
