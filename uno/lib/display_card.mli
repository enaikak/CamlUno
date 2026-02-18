open Card

val render_card : card -> string list
(** [render_card card] builds and colors the UNO card, returning a list of
    strings representing the visual display of the card. *)

val colored_label : card -> string
(** [colored_label card] returns a one-line label for the card where the
    value/special token is wrapped in an ANSI foreground color corresponding to
    the card's color. Useful for compact terminal hand displays. *)

val color_word : string -> string
(** [color_word color] returns [color] in uppercase wrapped in the ANSI
    foreground color corresponding to [color]. This is used to display the
    active color during gameplay. *)
