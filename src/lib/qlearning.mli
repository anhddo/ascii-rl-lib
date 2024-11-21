(* val init_environment : string -> Pytypes.pyobject
val train : Pytypes.pyobject -> int -> unit *)
include Base_algorithm.S
val value_to_bin : float -> float -> float -> int -> int
val convert_state_to_bin : float list -> int
val train : int -> unit
type bin = { low : float; high : float; num_bins : int }

val state_to_bin_config : bin list
val four_float_to_bin : float list -> bin list -> int list
val q_table : float array array