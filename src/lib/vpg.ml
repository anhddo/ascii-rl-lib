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

  let load_q_table (filename : string) =
    let file_content = Core.In_channel.read_all filename in
    Sexplib.Conv.array_of_sexp
      (Sexplib.Conv.array_of_sexp Sexplib.Conv.float_of_sexp)
      (Sexplib.Sexp.of_string file_content)

  let vpg_params =
    let file_name = Algo_config.model_path in
    if Sys.file_exists file_name then load_q_table file_name
    else
      Core.Array.make_matrix
        ~dimx:(int_of_float @@ (float_of_int state_bin ** float_of_int obs_dim))
        ~dimy:action_dim 0.0

  let save_model () =
    let sexp_str =
      Core.Sexp.to_string_hum
        (Core.Array.sexp_of_t
           (Core.Array.sexp_of_t Core.Float.sexp_of_t)
           vpg_params)
    in
    Core.Out_channel.write_all Algo_config.model_path ~data:sexp_str

  let softmax (arr : float array) : float array =
    let max_elem = Array.fold_left max neg_infinity arr in
    let exps = Array.map (fun x -> exp (x -. max_elem)) arr in
    let sum_exps = Array.fold_left ( +. ) 0.0 exps in
    Array.map (fun x -> x /. sum_exps) exps

  let select_action (state : int) : int =
    let probs = softmax vpg_params.(state) in
    let cumulative_probs = Array.make (Array.length probs) 0.0 in
    cumulative_probs.(0) <- probs.(0);
    for i = 1 to Array.length probs - 1 do
      cumulative_probs.(i) <- cumulative_probs.(i - 1) +. probs.(i)
    done;
    let r = Random.float 1.0 in
    let rec find_action (i : int) : int = 
      if i >= Array.length cumulative_probs then
        Array.length cumulative_probs - 1
      else if r <= cumulative_probs.(i) then i
      else find_action (i + 1)
    in
    find_action 0

  let update_policy 
      (trajectories : ((int * int) * float) list) 
      (learning_rate : float) =
    let returns = Array.of_list (List.map snd trajectories) in
    let states_actions = Array.of_list (List.map fst trajectories) in
    for i = 0 to Array.length returns - 1 do
      let state, action = states_actions.(i) in
      let probs = softmax vpg_params.(state) in
      for a = 0 to action_dim - 1 do
        let grad =
          if a = action then 1.0 -. probs.(a)
          else -.probs.(a)
        in
        vpg_params.(state).(a) <-
        vpg_params.(state).(a) +. learning_rate *. grad *. returns.(i)
      done
    done

  let calculate_returns (rewards : float list) (gamma : float) : float list =   (* chronological order input and output*)
    let rec aux (acc : float) (returns : float list) = function
      | [] -> returns
      | r :: rs ->
          let g_t = r +. gamma *. acc in
          aux g_t (g_t :: returns) rs
    in
    aux 0.0 [] (List.rev rewards)

  let update_trajectories
      (trajectories : ((int * int) * float) list) 
      (rewards : float list) 
      (gamma : float) 
      : ((int * int) * float) list =
    let returns = calculate_returns (List.rev rewards) gamma in
    let returns = List.rev returns in
    let mean = List.fold_left (+.) 0.0 returns /. float_of_int (List.length returns) in
    let variance = List.fold_left (fun acc x -> acc +. (x -. mean) ** 2.0) 0.0 returns /. float_of_int (List.length returns) in
    let std_dev = sqrt (variance +. 1e-8) in
    let standardized_returns = List.map (fun r -> (r -. mean) /. std_dev) returns in
    List.map2 (fun (s_a, _) g_t -> (s_a, g_t)) trajectories standardized_returns
      
  let train (episode : int) =
    let learning_rate = 0.01 in
    let max_steps = 250 in
    let gamma = 0.99 in
    for _episode = 1 to episode do
      let state, internal_state = Env.reset () in
      let state_bin = State_action_env.convert_state_to_bin state in
      let rec run_step t state_bin trajectories rewards internal_state =
        if t >= max_steps then
          ()
          (* let total_reward = List.fold_left (+.) 0.0 rewards in *)
          (* Printf.printf "Episode %d Success: Total Reward: %f\n%!" _episode total_reward *)
        else
          let action = select_action state_bin in
          (* Printf.printf "Selected action: %d\n%!" action; *)
          let passing_action_to_env =
            match action_bin with
            | Discrete _ -> [ float_of_int action ]
            | Continuous x -> [ State_action_env.bin_to_value action x ]
          in
          let response = Env.step internal_state passing_action_to_env in
          let next_state = response.observation in
          let reward = response.reward in
          let is_done = response.terminated in
          let truncated = response.truncated in
          let next_state_bin = State_action_env.convert_state_to_bin next_state in
          let trajectories = ((state_bin, action), 0.0) :: trajectories in
          let rewards = reward :: rewards in
          (* Printf.printf "length trajectories %d\n rewards: %d\n" (List.length trajectories) (List.length rewards); *)
          if is_done || truncated then
            let updated_trajectories = update_trajectories trajectories rewards gamma in
            update_policy updated_trajectories learning_rate;
            let total_reward = List.fold_left (+.) 0.0 rewards in
            Printf.printf "Episode %d: Total Reward: %f\n%!" _episode total_reward
          else
            Env.render response.internal_state;
            run_step (t + 1) next_state_bin trajectories rewards response.internal_state
      in
      run_step 0 state_bin [] [] internal_state
    done

end
