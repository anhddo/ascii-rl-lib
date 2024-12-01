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

module Make =
functor
  (M : Simulation.S)
  ->
  struct
    type bin_config = {
      low_list : float list;
      high_list : float list;
      num_bins : int;
    }

    type continuous_bin = { low : float; high : float; num_bins : int }
    type bin = Discrete of int | Continuous of continuous_bin

    type state_action_config = {
      obs_dim : int;
      action_dim : int;
      state_bin : int;
      action_bin : bin;
      is_continuous_action : bool;
    }

    let state_bin = 20

    let config =
      match M.env_type with
      | Cartpole ->
          {
            low_list = [ -4.8; -4.0; -0.418; -4.0 ];
            high_list = [ 4.8; 4.0; 0.418; 4.0 ];
            num_bins = state_bin;
          }
      | Pendulum ->
          {
            low_list = [ -1.; -1.; -8. ];
            high_list = [ 1.; 1.; 8. ];
            num_bins = state_bin;
          }
      | _ -> failwith "invalid environment"

    let q_config =
      match M.env_type with
      | Cartpole ->
          {
            obs_dim = 4;
            action_dim = 2;
            state_bin;
            is_continuous_action = false;
            action_bin = Discrete 2;
          }
      | Pendulum ->
          {
            obs_dim = 3;
            action_dim = 1;
            state_bin;
            action_bin = Continuous { low = -2.; high = 2.; num_bins = 7 };
            is_continuous_action = true;
          }
      | _ -> failwith "invalid environment name"

    let state_to_bin_config : bin list =
      Core.List.map2_exn config.low_list config.high_list ~f:(fun low high ->
          Continuous { low; high; num_bins = config.num_bins })

    let value_to_bin (value : float) (low : float) (high : float)
        (num_bins : int) : int =
      let bin_width = (high -. low) /. float_of_int num_bins in
      let bin = int_of_float ((value -. low) /. bin_width) in
      if bin >= num_bins then num_bins - 1 else if bin < 0 then 0 else bin

    let bin_to_value (bin : int) (bin_config : continuous_bin) : float =
      let bin_width =
        (bin_config.high -. bin_config.low) /. float_of_int bin_config.num_bins
      in
      (bin_width *. float_of_int bin) +. bin_config.low

    let convert_state_to_bin_list (state : float list) (bin_config : bin list) :
        int list =
      let lz = Core.List.zip_exn state bin_config in
      Core.List.fold lz ~init:[] ~f:(fun acc (s, b) ->
          match b with
          | Discrete _ -> failwith "discrete bin not supported"
          | Continuous { low; high; num_bins } ->
              value_to_bin s low high num_bins :: acc)
      |> List.rev

    let convert_state_to_bin (state : float list) : int =
      (* printf.printf "state: %s \n" (list.to_string ~f:float.to_string state); *)
      let state_bin_list =
        convert_state_to_bin_list state state_to_bin_config
      in
      let rec convert_state_to_bin' (state : int list) (n : int) : int =
        match state with
        | [] -> 0
        | h :: t ->
            int_of_float
              (float_of_int h
              *. Float.pow (float_of_int state_bin) (float_of_int n))
            + convert_state_to_bin' t (n - 1)
      in
      convert_state_to_bin' state_bin_list (List.length state_bin_list - 1)
  end
