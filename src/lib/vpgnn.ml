[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-33"]
open Torch

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

(* let num_states = int_of_float @@ Float.pow 20.0 4.0 *)

(* let policy_params = Array.make_matrix num_states num_actions 0.0 *)
let build_model input_size output_size hidden_size =
  let vs = Var_store.create ~name:"nn" () in
  let fc1 = Layer.linear vs ~input_dim:input_size hidden_size in
  let fc2 = Layer.linear vs ~input_dim:hidden_size output_size in
  object
    method forward input =
      input
      |> Layer.forward fc1
      |> Tensor.relu
      |> Layer.forward fc2
    method var_store = vs
  end
let input_size = 4
let output_size = 2
let hidden_size = 3
let model = build_model input_size output_size hidden_size

(* let build_model input_size hidden_size output_size =
  let vs = Var_store.create ~name:"nn" () in
  let fc1 = Layer.linear vs ~input_dim:input_size hidden_size in
  let fc2 = Layer.linear vs ~input_dim:hidden_size output_size in
  print_weights vs;
  (fc1, fc2, vs)
let forward (fc1 : Layer.t) (fc2 : Layer.t) input =
  input
  |> Layer.forward fc1
  |> Tensor.relu
  |> Layer.forward fc2
  |> Tensor.softmax ~dim:0 ~dtype:(Torch_core.Kind.T Float)
let input_size = 4
let hidden_size = 128
let output_size = 2
let fc1, fc2, vs = build_model input_size hidden_size output_size *)

(* let print_tensor_info tensor =
  let shape = Tensor.shape tensor in
  let shape_str = String.concat ", " (List.map string_of_int shape) in
  Printf.printf "Probs shape: [%s]\n%!" shape_str;
  Printf.printf "Probs content: %s\n%!" (Tensor.to_string tensor ~line_size:80) *)

(* let print_weights vs =
  Var_store.all_vars vs
  |> List.iter (fun (name, tensor) ->
          Printf.printf "Parameter: %s, Values: %s\n%!"
            name
            (Tensor.to_string tensor ~line_size:80)) *)

(* let print_gradients vs =
  Var_store.all_vars vs
  |> List.iter (fun (name, tensor) ->
          try
            let grad = Tensor.grad tensor in
            Printf.printf "Parameter: %s\nGradient: %s\n%!"
              name
              (Tensor.to_string grad ~line_size:80)
          with _ ->
            Printf.printf "Parameter: %s\nNo gradient available.\n%!" name) *)

let select_action model obs =
  let obs_tensor = Tensor.of_float1 obs |> Tensor.unsqueeze ~dim:0 in
  let probs = model#forward obs_tensor |> Tensor.softmax ~dim:(-1) ~dtype:(Torch_core.Kind.T Float) in
  let probs_ = Tensor.squeeze probs in
  (* print_tensor_info probs_; *)

  (* Sample an action based on the probabilities *)
  let action_tensor = Tensor.multinomial probs ~num_samples:1 ~replacement:true in
  let action = Tensor.select action_tensor ~dim:0 ~index:0 |> Tensor.int_value in
  (* Get the probability of the selected action *)
  let action_prob = Tensor.select probs ~dim:1 ~index:action in
  (action, action_prob)

let update_policy rewards probs optimizer =
  let log_probs = Tensor.log probs in
  (* print_tensor_info rewards;
  print_tensor_info log_probs; *)
  let loss = Tensor.neg (Tensor.sum (Tensor.mul log_probs rewards)) in
  Optimizer.zero_grad optimizer;
  Tensor.backward loss ~keep_graph:true;
  Optimizer.step optimizer;
  Printf.printf "Loss: %f\n%!" (Tensor.float_value loss)
  
let calculate_returns rewards gamma =
  let rec aux acc returns = function
    | [] -> returns
    | r :: rs ->
        let g_t = r +. gamma *. acc in
        aux g_t (g_t :: returns) rs
  in
  aux 0.0 [] (List.rev rewards)

let update_trajectories rewards gamma =
  let returns = calculate_returns rewards gamma in
  (* let print_rewards rewards =
    List.iter (fun r -> Printf.printf "%f " r) rewards;
    print_endline ""
  in
  print_rewards rewards;
  print_rewards returns;
  let length = List.length returns in
  Printf.printf "Length of returns: %d\n" length; *)

  let mean = List.fold_left (+.) 0.0 returns /. float_of_int (List.length returns) in
  let variance = List.fold_left (fun acc x -> acc +. (x -. mean) ** 2.0) 0.0 returns /. float_of_int (List.length returns) in
  let std_dev = sqrt (variance +. 1e-8) in
  let standardized_returns = List.map (fun r -> (r -. mean) /. std_dev) returns in
  let standardized_returns_tensor = Tensor.of_float1 (Array.of_list standardized_returns) in
  standardized_returns_tensor
  
let loop env =
  let num_episodes = 1000 in
  let max_steps = 200 in
  let learning_rate = 0.01 in
  let gamma = 0.7 in
  let optimizer = Optimizer.adam model#var_store ~learning_rate in
  for episode = 1 to num_episodes do
    let state = reset_fn env in
    let state = Array.of_list state in
    let rec run_step t state rewards probs =
      if t >= max_steps then
        Printf.printf "Episode %d Success: Time Steps: %d\n%!" episode t
      else
        let action, prob = select_action model state in
        (* Printf.printf "Selected action: %d\n%!" action; *)
        let probs =
          if Tensor.shape probs = [0] then
            prob
          else
            Tensor.cat [probs; prob] ~dim:0
        in
        (* print_endline "state : ";
        Array.iter (fun x -> Printf.printf "%f " x) state;
        Printf.printf "\n";
        print_endline "probs accumulated: ";
        print_tensor_info probs; *)

        let next_state, reward, is_done, _, _ = step_fn env action in
        let next_state = Array.of_list next_state in
        let rewards = reward :: rewards in
        if is_done then
          let rewards = update_trajectories rewards gamma in
          (* print_weights model#var_store; *)
          (* print_gradients model#var_store; *)
          update_policy rewards probs optimizer;
          Printf.printf "Episode %d: Time Steps: %d\n%!" episode t
        else
          run_step (t + 1) next_state rewards probs
    in
    let probs = Tensor.zeros [0] in
    run_step 0 state [] probs
  done

let () =
  loop env;
  (* loop env_render *)