module Make : functor (_ : Base_algorithm.Algo_config) (_ : Simulation.S) -> sig
  val vpg_params : float array array
  val train : int -> unit
  val save_model : unit -> unit
end