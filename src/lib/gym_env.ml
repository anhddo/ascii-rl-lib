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
      let result =
        Py.Callable.to_function
          (Core.Option.value_exn step_fn')
          [| Py.Int.of_int @@ int_of_float (List.hd action) |]
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

  type bin = { low : float; high : float; num_bins : int }

  let config =
    match M.name with
    | "CartPole-v1" ->
        {
          low_list = [ -4.8; -4.0; -0.418; -4.0 ];
          high_list = [ 4.8; 4.0; 0.418; 4.0 ];
          num_bins = 20;
        }
    | "Pendulum-v1" ->
        {
          low_list = [ -1.; -1.; -8.];
          high_list = [ 1.; 1.; 8. ];
          num_bins = 20;
        }
    | _ -> failwith "Invalid environment name"

  let state_to_bin_config : bin list =
    List.map2_exn config.low_list config.high_list ~f:(fun low high ->
        { low; high; num_bins = config.num_bins })

  let value_to_bin (value : float) (low : float) (high : float) (num_bins : int)
      : int =
    if Float.compare value low = -1 then 0
    else if Float.compare value high = 1 then num_bins - 1
    else
      let bin_width = (high -. low) /. float_of_int num_bins in
      let bin = (value -. low) /. bin_width in
      int_of_float bin

  let rec four_float_to_bin (state : float list) (bin_config : bin list) :
      int list =
    match (state, bin_config) with
    | sh :: st, bh :: bt ->
        value_to_bin sh bh.low bh.high bh.num_bins :: four_float_to_bin st bt
    | [], [] -> []
    | _ -> failwith "State and bin configuration lengths do not match"

  let convert_state_to_bin (state : float list) : int =
    let state_bin = four_float_to_bin state state_to_bin_config in
    let rec convert_state_to_bin' (state : int list) (n : int) : int =
      match state with
      | [] -> 0
      | h :: t ->
          int_of_float (float_of_int h *. Float.( ** ) 20.0 (float_of_int n))
          + convert_state_to_bin' t (n - 1)
    in
    convert_state_to_bin' state_bin 3
end
