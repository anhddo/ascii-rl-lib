[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

module Make =
functor
  (Config : Simulation.Config)
  ->
  struct
    (* if not initialize then initialized*)
    if not (Py.is_initialized ()) then Py.initialize ()

    type t = float list (* Length is 2 | [location, ang_speed ] *)
    type action = float list

    type response = {
      observation : t;
      reward : float;
      terminated : bool;
      truncated : bool;
      info : string;
    }

    (*if not inialize then initialize*)
    type action_type = Discrete | Continuous

    let action_type =
      match Config.name with
      | "CartPole-v1" -> Discrete
      | "Pendulum-v1" -> Continuous
      | _ -> failwith "Invalid environment name"

    let gym = Py.import "gymnasium"

    let init_environment (str : string) (render : bool) =
      if render then
        Py.Module.get_function_with_keywords gym "make"
          [| Py.String.of_string str |]
          [ ("render_mode", Py.String.of_string "human") ]
      else Py.Module.get_function gym "make" [| Py.String.of_string str |]

    let env = init_environment Config.name Config.render
    let create () : t = [ 0.; 0. ]

    let reset () : t =
      let reset_fn' = Py.Object.get_attr_string env "reset" in
      let result =
        Py.Callable.to_function (Core.Option.value_exn reset_fn') [||]
      in
      let result, _ = Py.Tuple.to_tuple2 result in
      (*convert to list of float*)
      let state = Py.List.to_list_map Py.Float.to_float result in
      state

    let step (state : t) (action : action) =
      let step_fn' = Py.Object.get_attr_string env "step" in
      let passing_action =
        match action_type with
        | Discrete -> [| Py.Int.of_int @@ int_of_float (List.hd action) |]
        | Continuous -> [| Py.List.of_list_map Py.Float.of_float action |]
      in
      let result =
        Py.Callable.to_function (Core.Option.value_exn step_fn') passing_action
      in
      let _state, _reward, _is_done, _truncated, _ =
        Py.Tuple.to_tuple5 result
      in
      {
        observation = Py.List.to_list_map Py.Float.to_float _state;
        reward = Py.Float.to_float _reward;
        terminated = Py.Bool.to_bool _is_done;
        truncated = false;
        info = "";
      }
    (* ( Py.List.to_list_map Py.Float.to_float _state,
       Py.Float.to_float _reward,
       Py.Bool.to_bool _is_done,
       false,
       "" ) *)

    let render (x : t) = [ 'a'; 'b' ]
  end

open Core

module Make_config (M : Simulation.Config) = struct
  type qconfig = {
    low_list : float list;
    high_list : float list;
    num_bins : int;
  }

  type continuous_bin = { low : float; high : float; num_bins : int }
  type bin = Discrete of int | Continuous of continuous_bin

  type q_table_config = {
    obs_dim : int;
    action_dim : int;
    state_bin : int;
    action_bin : bin;
    is_continuous_action : bool;
  }

  let state_bin = 20

  let config =
    match M.name with
    | "CartPole-v1" ->
        {
          low_list = [ -4.8; -4.0; -0.418; -4.0 ];
          high_list = [ 4.8; 4.0; 0.418; 4.0 ];
          num_bins = state_bin;
        }
    | "Pendulum-v1" ->
        {
          low_list = [ -1.; -1.; -8. ];
          high_list = [ 1.; 1.; 8. ];
          num_bins = state_bin;
        }
    | _ -> failwith "Invalid environment name"

  let q_config =
    match M.name with
    | "CartPole-v1" ->
        {
          obs_dim = 4;
          action_dim = 2;
          state_bin;
          is_continuous_action = false;
          action_bin = Discrete 2;
        }
    | "Pendulum-v1" ->
        {
          obs_dim = 3;
          action_dim = 1;
          state_bin;
          action_bin = Continuous { low = -1.; high = 1.; num_bins = 7 };
          is_continuous_action = true;
        }
    | _ -> failwith "Invalid environment name"

  let state_to_bin_config : bin list =
    List.map2_exn config.low_list config.high_list ~f:(fun low high ->
        Continuous { low; high; num_bins = config.num_bins })

  let value_to_bin (value : float) (low : float) (high : float) (num_bins : int)
      : int =
    if Float.compare value low = -1 then 0
    else if Float.compare value high = 1 then num_bins - 1
    else
      let bin_width = (high -. low) /. float_of_int num_bins in
      let bin = (value -. low) /. bin_width in
      int_of_float bin

  let bin_to_value (bin : int) (bin_config : continuous_bin) : float =
    let bin_width =
      (bin_config.high -. bin_config.low) /. float_of_int bin_config.num_bins
    in
    (bin_width *. float_of_int bin) +. bin_config.low

  let convert_state_to_bin_list (state : float list) (bin_config : bin list) :
      int list =
    let lz = List.zip_exn state bin_config in
    List.fold lz ~init:[] ~f:(fun acc (s, b) ->
        match b with
        | Discrete _ -> failwith "Discrete bin not supported"
        | Continuous { low; high; num_bins } ->
            value_to_bin s low high num_bins :: acc)
    |> List.rev

  (* print state_to_bin_config *)

  let convert_state_to_bin (state : float list) : int =
    let state_bin_list = convert_state_to_bin_list state state_to_bin_config in
    let rec convert_state_to_bin' (state : int list) (n : int) : int =
      match state with
      | [] -> 0
      | h :: t ->
          int_of_float
            (float_of_int h
            *. Float.( ** ) (float_of_int state_bin) (float_of_int n))
          + convert_state_to_bin' t (n - 1)
    in
    let result =
      convert_state_to_bin' state_bin_list (List.length state_bin_list - 1)
    in
    Printf.printf "state: %s\n"
      (List.to_string state ~f:Float.to_string);
    Printf.printf "state_bin_list: %s\n"
      (List.to_string state_bin_list ~f:Int.to_string);
    Printf.printf "bin: %d\n" result;
    result
end
