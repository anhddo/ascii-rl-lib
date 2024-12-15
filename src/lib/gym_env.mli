(* A Testing Enviroment from Python's Gymnasium*)

module Make (_ : Simulation.Config) : sig
  include Simulation.S

  val reset : unit -> t
  val step : t -> action -> response
  val render : t -> char list
end

