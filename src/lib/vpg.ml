(* a backup version for vpg without nn*)
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-33"]

let () = Py.initialize () ~interpreter:"/Users/shenyang/opt/anaconda3/envs/pytorch/bin/python"

let gym = Py.import "gymnasium"

(* let env_render =
  Py.Module.get_function_with_keywords gym "make"
    [| Py.String.of_string "CartPole-v1" |]
    [ ("render_mode", Py.String.of_string "human") ] *)

let env =
  Py.Module.get_function gym "make" [| Py.String.of_string "CartPole-v1" |]

let reset_fn env =
  let reset_fn' = Py.Object.get_attr_string env "reset" in
  let result = Py.Callable.to_function (Option.get reset_fn') [||] in
  let result, _ = Py.Tuple.to_tuple2 result in
  let state = Py.List.to_list_map Py.Float.to_float result in
  state

let step_fn env action =
  let step_fn' = Py.Object.get_attr_string env "step" in
  let result =
    Py.Callable.to_function
      (Option.get step_fn')
      [| Py.Int.of_int @@ action |]
  in
  let _state, _reward, _is_done, _truncated, _ = Py.Tuple.to_tuple5 result in
  ( Py.List.to_list_map Py.Float.to_float _state,
    Py.Float.to_float _reward,
    Py.Bool.to_bool _is_done,
    _truncated,
    None )

let value_to_bin (value : float) (low : float) (high : float) (num_bins : int) :
    int =
  if value < low then 0
  else if value > high then num_bins - 1
  else
    let bin_width = (high -. low) /. float_of_int num_bins in
    let bin = (value -. low) /. bin_width in
    int_of_float bin

type bin = { low : float; high : float; num_bins : int }

let state_to_bin_config : bin list =
  [
    { low = -4.8; high = 4.8; num_bins = 20 };
    { low = -4.0; high = 4.0; num_bins = 20 };
    { low = -0.418; high = 0.418; num_bins = 20 };
    { low = -4.0; high = 4.0; num_bins = 20 };
  ]

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
        h * int_of_float (Float.pow 8.0 (float_of_int n))
        + convert_state_to_bin' t (n - 1)
  in
  convert_state_to_bin' state_bin 3

let num_states = int_of_float @@ Float.pow 20.0 4.0
let num_actions = 2

let policy_params = Array.make_matrix num_states num_actions 0.0

let softmax arr =
  let max_elem = Array.fold_left max neg_infinity arr in
  let exps = Array.map (fun x -> exp (x -. max_elem)) arr in
  let sum_exps = Array.fold_left ( +. ) 0.0 exps in
  Array.map (fun x -> x /. sum_exps) exps

let select_action state =
  let probs = softmax policy_params.(state) in
  let cumulative_probs = Array.make (Array.length probs) 0.0 in
  cumulative_probs.(0) <- probs.(0);
  for i = 1 to Array.length probs - 1 do
    cumulative_probs.(i) <- cumulative_probs.(i - 1) +. probs.(i)
  done;
  let r = Random.float 1.0 in
  let rec find_action i = 
    if i >= Array.length cumulative_probs then
      Array.length cumulative_probs - 1
    else if r <= cumulative_probs.(i) then i
    else find_action (i + 1)
  in
  find_action 0

let update_policy trajectories learning_rate =
  let returns = Array.of_list (List.map snd trajectories) in
  let states_actions = Array.of_list (List.map fst trajectories) in
  let total_return = Array.fold_left ( +. ) 0.0 returns in
  for i = 0 to Array.length returns - 1 do
    let state, action = states_actions.(i) in
    let probs = softmax policy_params.(state) in
    for a = 0 to num_actions - 1 do
      let grad =
        if a = action then 1.0 -. probs.(a)
        else -.probs.(a)
      in
      policy_params.(state).(a) <-
        policy_params.(state).(a) +. learning_rate *. grad *. returns.(i)
    done
  done
  
let calculate_returns rewards gamma =
  let rec aux acc returns = function
    | [] -> List.rev returns
    | r :: rs ->
        let g_t = r +. gamma *. acc in
        aux g_t (g_t :: returns) rs
  in
  aux 0.0 [] (List.rev rewards)

(* let update_trajectories trajectories rewards gamma =
  let returns = calculate_returns rewards gamma in
  List.map2 (fun (s_a, _) g_t -> (s_a, g_t)) trajectories returns *)

let update_trajectories trajectories rewards gamma =
  let returns = calculate_returns rewards gamma in
  (* 标准化 returns *)
  let mean = List.fold_left (+.) 0.0 returns /. float_of_int (List.length returns) in
  let variance = List.fold_left (fun acc x -> acc +. (x -. mean) ** 2.0) 0.0 returns /. float_of_int (List.length returns) in
  let std_dev = sqrt (variance +. 1e-8) in
  let standardized_returns = List.map (fun r -> (r -. mean) /. std_dev) returns in
  List.map2 (fun (s_a, _) g_t -> (s_a, g_t)) trajectories standardized_returns
  
let loop env =
  let num_episodes = 1000 in  (*1000*)
  let max_steps = 200 in    (*200*)
  let learning_rate = 0.05 in
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
  

let () =
  loop env;
  (* loop env_render *)