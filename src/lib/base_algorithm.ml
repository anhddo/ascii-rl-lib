module type S = sig
    val init_environment : string -> bool -> Pytypes.pyobject
    val train : Pytypes.pyobject -> int -> unit
end