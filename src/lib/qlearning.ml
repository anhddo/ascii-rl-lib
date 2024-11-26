open Core

let choose_action (_ : float list) : float list = [ 0.0 ]

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
          action_bin = Continuous { low = -2.; high = 2.; num_bins = 7 };
          is_continuous_action = true;
        }
    | _ -> failwith "Invalid environment name"

  let state_to_bin_config : bin list =
    List.map2_exn config.low_list config.high_list ~f:(fun low high ->
        Continuous { low; high; num_bins = config.num_bins })

  let value_to_bin (value : float) (low : float) (high : float) (num_bins : int)
      : int =
    if Float.compare value low = -1 then 0
    else if Float.compare value high <> -1 then num_bins - 1
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

  let convert_state_to_bin (state : float list) : int =
    (* Printf.printf "state: %s \n" (List.to_string ~f:Float.to_string state); *)
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
    convert_state_to_bin' state_bin_list (List.length state_bin_list - 1)
end

module Make (Env_config : Simulation.Config) = struct
  module QLearningConfig = Make_config (Env_config)

  (* module Env = Gym_env.Make (Env_config) *)
  module Env = Pendulum

  let state_bin = QLearningConfig.q_config.state_bin
  let action_bin = QLearningConfig.q_config.action_bin
  let obs_dim = QLearningConfig.q_config.obs_dim

  let action_dim =
    match action_bin with Discrete n -> n | Continuous x -> x.num_bins

  let argmax (arr : 'a list) ~(compare : 'a -> 'a -> int) ~(init : 'a) : int =
    let rec loop' (arr : 'a list) (max : 'a) (index : int) (i : int) =
      match arr with
      | [] -> index
      | hd :: tl ->
          if compare hd max = 1 then loop' tl hd i (i + 1)
          else loop' tl max index (i + 1)
    in
    loop' arr init 0 0

  let float_argmax (arr : float list) : int =
    argmax arr ~compare:Float.compare ~init:Float.neg_infinity

  let load_q_table (filename : string) =
    let file_content = In_channel.read_all filename in
    Sexplib.Conv.array_of_sexp
      (Sexplib.Conv.array_of_sexp Sexplib.Conv.float_of_sexp)
      (Sexplib.Sexp.of_string file_content)

  let q_table =
    let file_name = Env_config.q_table_path in
    if Sys_unix.file_exists_exn file_name then load_q_table file_name
    else
      Array.make_matrix
        ~dimx:(int_of_float @@ (float_of_int state_bin ** float_of_int obs_dim))
        ~dimy:action_dim 0.0

  let save_q_table () =
    let sexp_str =
      Sexp.to_string_hum
        (Array.sexp_of_t (Array.sexp_of_t Float.sexp_of_t) q_table)
    in
    Out_channel.write_all Env_config.q_table_path ~data:sexp_str

  let train (episode : int) =
    let learning_rate = 0.1 in

    let rec loop' (episode : int) (state : float list)
        (internal_state : float list) (reward_ep : float) =
      let action =
        if Float.compare (Random.float 1.0) 0.1 = -1 then Random.int action_dim
        else
          let state_bin = QLearningConfig.convert_state_to_bin state in
          q_table.(state_bin) |> Array.to_list |> float_argmax
      in
      let passing_action_to_env =
        match action_bin with
        | Discrete _ -> [ float_of_int action ]
        | Continuous x -> [ QLearningConfig.bin_to_value action x ]
      in
      let response = Env.step internal_state passing_action_to_env in
      (* Printf.printf "internal_state: %s \n" (List.to_string ~f:Float.to_string response.internal_state);
         Printf.printf "truncate: %b \n" response.truncated; *)
      let next_state = response.observation in
      let reward = response.reward in
      let is_done = response.terminated in
      let truncated = response.truncated in
      let next_state_bin = QLearningConfig.convert_state_to_bin next_state in
      let state_bin = QLearningConfig.convert_state_to_bin state in
      (* update q table*)
      let _ =
        if is_done || truncated then q_table.(state_bin).(action) <- reward
        else
          let max_q =
            List.max_elt
              (q_table.(next_state_bin) |> Array.to_list)
              ~compare:Float.compare
            |> Option.value_exn
            (* max q_table.(next_state_bin).(0) q_table.(next_state_bin).(1) *)
          in
          q_table.(state_bin).(action) <-
            ((1. -. learning_rate) *. (reward +. (0.99 *. max_q)))
            +. (learning_rate *. q_table.(state_bin).(action))
      in
      let state = next_state in
      let reward_ep = reward_ep +. reward in
      if is_done || truncated then (
        if episode mod 100 = 0 then
          Printf.printf "episode %d reward_ep: %f\n" episode reward_ep;
        (* Printf.printf "Episode: %d, done %b truncated: %b, reward: %f\n" *)
        (* episode is_done truncated reward; *)
        let state, internal_state = Env.reset () in
        (* report total reward *)
        loop' (episode - 1) state internal_state 0.0)
      else if episode > 0 then (
        if Env_config.render then Env.render response.internal_state;
        loop' episode state response.internal_state reward_ep)
      else ()
      (* else q_table *)
    in
    let state, internal_state = Env.reset () in
    loop' episode state internal_state 0.0
end
