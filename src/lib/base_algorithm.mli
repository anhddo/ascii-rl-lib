module type S = sig
  (* val init_environment : string -> bool -> Pytypes.pyobject *)
  (* val train : Pytypes.pyobject -> int -> unit *)
  val choose_action : float list -> float list
end

module type Algo_config = sig
  val model_path : string
end