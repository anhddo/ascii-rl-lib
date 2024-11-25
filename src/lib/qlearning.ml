open Core

let choose_action (_ : float list) : float list = [ 0.0 ]

module Make (Env_config : Simulation.Config) = struct
  module QLearningConfig = Gym_env.Make_config (Env_config)
  module Env = Gym_env.Make (Env_config)

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

    let rec loop' (episode : int) (state : float list) (reward_ep : float) =
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
      let response = Env.step state passing_action_to_env in
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
        let state = Env.reset () in
        (* report total reward *)
        loop' (episode - 1) state 0.0)
      else if episode > 0 then loop' episode state reward_ep
      else ()
      (* else q_table *)
    in
    loop' episode (Env.reset ()) 0.0
end
