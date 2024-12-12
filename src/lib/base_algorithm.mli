module type Algo_config = sig
  val algo_name : string
  val model_path : string
  val episode : int
  val learning_rate : float
end

module type Algo_base = sig
  val train : int -> unit
  val save_model : unit -> unit
end

