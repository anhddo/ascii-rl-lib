(* val init_environment : string -> Pytypes.pyobject
   val train : Pytypes.pyobject -> int -> unit *)
include Base_algorithm.S


module Make (_ : Simulation.Config) : sig
  val train : int -> unit
end
