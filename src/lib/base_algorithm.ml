module type T = sig
    val init_environment : string -> Pytypes.pyobject
    val train : Pytypes.pyobject -> int -> unit
end