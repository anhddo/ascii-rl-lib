(* A Testing Enviroment from Python's Gymnasium*)

module Make : functor (_ : Simulation.Config) -> sig
  include Simulation.T

  val reset : unit -> t
  val step : t -> action -> response
  val render : t -> char list
end

