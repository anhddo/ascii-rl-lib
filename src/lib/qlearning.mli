module Make : functor (_ : Base_algorithm.Algo_config) (_ : Simulation.S) -> sig
  val q_table : float array array
  val train : int -> unit
  val save_q_table : unit -> unit
end
