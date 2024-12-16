(* Blackjack is a Simulation *)

module Make (_ : Simulation.Config) : sig
    include Simulation.S
  
    val draw_card : unit -> float
    val simulate : unit -> unit
  end
