module type Algo_config = sig
  val algo_name : string
  val model_path : string
  val episode : int
  val learning_rate : float
  val gamma : float
end

module type Algo_base = sig
  val train : unit -> unit
  val save_model : unit -> unit
end
