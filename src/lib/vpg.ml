module type Algo_config = sig
  val model_path : string
end

module Make (Algo_config : Algo_config) (Env : Simulation.S) = struct
  (* module State_action_env = Make_config (Env_config) *)

  (* module Env = Gym_env.Make (Env_config) *)
  module State_action_env = State_action.Make (Env)

  let state_bin = State_action_env.q_config.state_bin
  let action_bin = State_action_env.q_config.action_bin
  let obs_dim = State_action_env.q_config.obs_dim

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
    let file_content = Core.In_channel.read_all filename in
    Sexplib.Conv.array_of_sexp
      (Sexplib.Conv.array_of_sexp Sexplib.Conv.float_of_sexp)
      (Sexplib.Sexp.of_string file_content)

  let q_table =
    let file_name = Algo_config.model_path in
    if Sys.file_exists file_name then load_q_table file_name
    else
      Core.Array.make_matrix
        ~dimx:(int_of_float @@ (float_of_int state_bin ** float_of_int obs_dim))
        ~dimy:action_dim 0.0

  let save_q_table () =
    let sexp_str =
      Core.Sexp.to_string_hum
        (Core.Array.sexp_of_t
           (Core.Array.sexp_of_t Core.Float.sexp_of_t)
           q_table)
    in
    Core.Out_channel.write_all Algo_config.model_path ~data:sexp_str

  let train (episode : int) =
    let learning_rate = 0.05 in
    let num_episodes = 1000 in
    let max_steps = 400 in
    let gamma = 0.99 in
    for episode = 1 to num_episodes do
      let state = reset_fn env in
      let state_bin = convert_state_to_bin state in
      let rec run_step t state_bin trajectories rewards =
        if t >= max_steps then
          let total_reward = List.fold_left (+.) 0.0 rewards in
          Printf.printf "Episode %d Success: Total Reward: %f\n%!" episode total_reward
        else
          let action = select_action state_bin in
          (* Printf.printf "Selected action: %d\n%!" action; *)
          let next_state, reward, is_done, _, _ = step_fn env action in
          let next_state_bin = convert_state_to_bin next_state in
          let trajectories = ((state_bin, action), 0.0) :: trajectories in
          let rewards = reward :: rewards in
          (* Printf.printf "length trajectories %d\n rewards: %d\n" (List.length trajectories) (List.length rewards); *)
          if is_done then
            let updated_trajectories = update_trajectories trajectories rewards gamma in
            update_policy updated_trajectories learning_rate;
            let total_reward = List.fold_left (+.) 0.0 rewards in
            Printf.printf "Episode %d: Total Reward: %f\n%!" episode total_reward
          else
            run_step (t + 1) next_state_bin trajectories rewards
      in
      run_step 0 state_bin [] []
    done

end
