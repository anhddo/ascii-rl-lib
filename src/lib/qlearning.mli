include Base_algorithm.S

module type Algo_config = sig
  val model_path : string
end

module Make : functor (_ : Algo_config) (_ : Simulation.S) -> sig
  val q_table : float array array
  val train : int -> unit
  val save_q_table : unit -> unit
end
