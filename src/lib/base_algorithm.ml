module type S = sig

    (* Initiatize a testing environment using PyML *)
    val init_environment : string -> Pytypes.pyobject

    (* Begin using a testing environment using PyML *)
    val train : Pytypes.pyobject -> int -> unit
end