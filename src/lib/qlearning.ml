[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-33"]

(* let gym = Py.import "gymnasium" *)

(* implement tabular q learning *)

(* type bin = { low : float; high : float; num_bins : int } *)

(* let state_to_bin_config : bin list =
  [
    { low = -4.8; high = 4.8; num_bins = 20 };
    { low = -4.0; high = 4.0; num_bins = 20 };
    { low = -0.418; high = 0.418; num_bins = 20 };
    { low = -4.0; high = 4.0; num_bins = 20 };
  ] *)


let q_table = Array.make_matrix (int_of_float @@ Float.pow 20. 4.) 2 0.0
let choose_action (state : float list) : float list = [ 0.0 ]

(* module Config = struct
     let name = "CartPole-v1"
     let render = false
   end

   module Env1 = Gym_env.Make (Config) *)

module Make (Env_config : Simulation.Config) = struct
  module QLearningConfig = Gym_env.Make_config (Env_config)
  module Env = Gym_env.Make (Env_config)
  let train (episode : int) =
    (* let env = env_render in *)
    let learning_rate = 0.1 in

    (* let state = reset_fn env in *)
    (* let dimx = int_of_float @@ Float.pow 20. 4. in *)
    let rec loop' (episode : int) (state : float list) (reward_ep : float) =
      let action =
        if Float.compare (Random.float 1.0) 0.1 = -1 then Random.int 2
        else
          let state_bin = QLearningConfig.convert_state_to_bin state in
          if q_table.(state_bin).(0) > q_table.(state_bin).(1) then 0 else 1
      in
      let response = Env.step state [ float_of_int action ] in
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
            max q_table.(next_state_bin).(0) q_table.(next_state_bin).(1)
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
