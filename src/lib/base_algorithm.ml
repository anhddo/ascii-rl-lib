module type S = sig
    val init_environment : string -> Pytypes.pyobject
    val train : Pytypes.pyobject -> int -> unit
end