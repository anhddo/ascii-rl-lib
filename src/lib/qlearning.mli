(* val init_environment : string -> Pytypes.pyobject
   val train : Pytypes.pyobject -> int -> unit *)
include Base_algorithm.S


module Make (_ : Simulation.Config) : sig
  val q_table : float array array
  val train : int -> unit
end
