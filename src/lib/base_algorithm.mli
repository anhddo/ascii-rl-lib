module type Algo_config = sig
  val model_path : string
  (* val episode: int (we will incorporate these latter)
     val learning_rate: float *)
end

module type Algo_base = sig
  val train : int -> unit
  val save_model : unit -> unit
end
