module type S = sig
  (* val init_environment : string -> bool -> Pytypes.pyobject *)
  (* val train : Pytypes.pyobject -> int -> unit *)
  val choose_action : float list -> float list
end

(* module type QLearning_config = sig
  val alpha : float
  val gamma : float
  val epsilon : float
  val num_episodes : int
  val max_steps : int

end *)