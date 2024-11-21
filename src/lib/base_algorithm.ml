module type S = sig
  (* Initiatize a testing environment using PyML *)
  (* val init_environment : string -> bool -> Pytypes.pyobject *)
  (* val train : Pytypes.pyobject -> int -> unit *)
  val choose_action : float list -> float list
end
