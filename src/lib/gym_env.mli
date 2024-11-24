(* A Testing Enviroment from Python's Gymnasium*)

module Make : functor (_ : Simulation.Config) -> sig
  include Simulation.T

  val reset : unit -> t
  val step : t -> action -> response
  val render : t -> char list
end

module Make_config (_ : Simulation.Config) : sig
  val value_to_bin : float -> float -> float -> int -> int
  val convert_state_to_bin : float list -> int

  (* val train : int -> unit *)
  (* type bin = { low : float; high : float; num_bins : int }

     val state_to_bin_config : bin list
     val four_float_to_bin : float list -> bin list -> int list
     val q_table : float array array *)
end
