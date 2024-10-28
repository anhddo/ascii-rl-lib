module type simulation =
  sig
    type action = int
    type response = { observation : int list; reward : int; terminated : bool; truncated : bool; info : string }
    val reset : int -> unit
    val step : action -> response
  end
