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

  let q_table =
    Array.make_matrix
      ~dimx:(int_of_float @@ (float_of_int state_bin ** float_of_int obs_dim))
      ~dimy:action_dim 0.0

  let train (episode : int) =
    (* Printf.printf "%d \n" (Array.length q_table); *)
    let learning_rate = 0.1 in

    let rec loop' (episode : int) (state : float list) (reward_ep : float) =
      let action =
        if Float.compare (Random.float 1.0) 0.1 = -1 then Random.int action_dim
        else
          let state_bin = QLearningConfig.convert_state_to_bin state in
          (* print_int state_bin; *)
          q_table.(state_bin) |> Array.to_list |> float_argmax
      in
      let passing_action_to_env =
        match action_bin with
        | Discrete _ -> [ float_of_int action ]
        | Continuous x -> [ QLearningConfig.bin_to_value action x ]
      in
      (* Printf.printf "action %s\n" (List.to_string passing_action_to_env ~f:Float.to_string); *)
      let response = Env.step state passing_action_to_env in
      let next_state = response.observation in
      let reward = response.reward in
      let is_done = response.terminated in
      let next_state_bin = QLearningConfig.convert_state_to_bin next_state in
      let state_bin = QLearningConfig.convert_state_to_bin state in
      (* update q table*)
      let _ =
        if is_done then q_table.(state_bin).(action) <- reward
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
      (* Printf.printf "%f \n " reward_ep; *)
      let reward_ep = reward_ep +. reward in
      (* Printf.printf "%b %f %f \n " is_done reward reward_ep; *)
      if is_done then (
        if episode mod 100 = 0 then
          Printf.printf "total reward:%d %f \n" episode reward_ep;
        let state = Env.reset () in
        (* report total reward *)
        loop' (episode - 1) state 0.0)
      else if episode > 0 then loop' episode state reward_ep
      else ()
      (* else q_table *)
    in
    loop' episode (Env.reset ()) 0.0
end
