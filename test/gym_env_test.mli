module Config :
  sig val q_table_path : string val name : string val render : bool end
module Cartpole_config :
  sig
    type continuous_bin =
      Gym_env.Make_config(Config).continuous_bin = {
      low : float;
      high : float;
      num_bins : int;
    }
    type bin =
      Gym_env.Make_config(Config).bin =
        Discrete of int
      | Continuous of continuous_bin
    type q_table_config =
      Gym_env.Make_config(Config).q_table_config = {
      obs_dim : int;
      action_dim : int;
      state_bin : int;
      action_bin : bin;
      is_continuous_action : bool;
    }
    val q_config : q_table_config
    val value_to_bin : float -> float -> float -> int -> int
    val convert_state_to_bin : float list -> int
    val bin_to_value : int -> continuous_bin -> float
  end
module Config1 :
  sig val q_table_path : string val name : string val render : bool end
module Pendulum_config :
  sig
    type continuous_bin =
      Gym_env.Make_config(Config1).continuous_bin = {
      low : float;
      high : float;
      num_bins : int;
    }
    type bin =
      Gym_env.Make_config(Config1).bin =
        Discrete of int
      | Continuous of continuous_bin
    type q_table_config =
      Gym_env.Make_config(Config1).q_table_config = {
      obs_dim : int;
      action_dim : int;
      state_bin : int;
      action_bin : bin;
      is_continuous_action : bool;
    }
    val q_config : q_table_config
    val value_to_bin : float -> float -> float -> int -> int
    val convert_state_to_bin : float list -> int
    val bin_to_value : int -> continuous_bin -> float
  end
val test_func : int -> int -> unit
val tests : OUnitTest.test
