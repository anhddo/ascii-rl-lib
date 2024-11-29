module type State_action = sig
  type continuous_bin = { low : float; high : float; num_bins : int }
  type bin = Discrete of int | Continuous of continuous_bin

  type state_action_config = {
    obs_dim : int;
    action_dim : int;
    state_bin : int;
    action_bin : bin;
    is_continuous_action : bool;
  }

  val state_to_bin_config : bin list
  val q_config : state_action_config
  val value_to_bin : float -> float -> float -> int -> int
  val convert_state_to_bin : float list -> int
  val convert_state_to_bin_list : float list -> bin list -> int list
  val bin_to_value : int -> continuous_bin -> float
end

module Make (_ : Simulation.S) : State_action
