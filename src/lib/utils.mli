open Torch

type nn_model = {
  var_store : Var_store.t;
  forward : Tensor.t -> Tensor.t;
}

val argmax : 'a list -> compare:('a -> 'a -> int) -> init:'a -> int

val float_argmax : float list -> int

val softmax : float array -> float array

val calculate_returns : float list -> float -> float list

val normalize_angle : float -> float

val modulo : float -> float -> float

val square : float -> float

val random_between : float -> float -> float

val clip : 'a -> 'a -> 'a -> 'a
