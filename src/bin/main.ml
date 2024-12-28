(* Main entry point for the reinforcement learning library *)

(* Configuration type *)
type config = {
  algo : string; (* algorithm to run *)
  simulation : string; (* simulation to run *)
  episode : int; (* number of episodes *)
  learning_rate : float; (* learning rate for the algorithm *)
  gamma : float; (* discount factor *)
  model_path : string; (* path to save the model *)
  render : bool; (* render the simulation *)
}

(* Default configuration *)
let default_config =
  {
    episode = 0;
    learning_rate = 0.1;
    gamma = 0.99;
    model_path = "";
    render = false;
    simulation = "pendulum";
    algo = "qlearning";
  }

(* Command-line argument specification *)
let parse_args () =
  let rec update_config config args =
    match args with
    | [] -> config
    | "--episode" :: value :: rest ->
        let episode = int_of_string value in
        update_config { config with episode } rest
    | "--simulation" :: value :: rest ->
        update_config { config with simulation = value } rest
    | "--algo" :: value :: rest ->
        update_config { config with algo = value } rest
    | "--model-path" :: value :: rest ->
        update_config { config with model_path = value } rest
    | "--learning-rate" :: value :: rest ->
        update_config { config with learning_rate = float_of_string value } rest
    | "--gamma" :: value :: rest ->
        update_config { config with gamma = float_of_string value } rest
    | "--render" :: rest -> update_config { config with render = true } rest
    | _ :: rest -> update_config config rest
  in
  update_config default_config (Array.to_list Sys.argv |> List.tl)

(* Entry point *)
let () =
  let config = parse_args () in
  let module Sim_env = struct
    let simulation_name = config.simulation
    let render = config.render
  end in
  let module Algo_config = struct
    let algo_name = config.algo
    let model_path = config.model_path
    let episode = config.episode
    let learning_rate = config.learning_rate
    let gamma = config.gamma
  end in
  (* Choose the simulation modules *)
  let module Sim = struct
    include
      (val match Sim_env.simulation_name with
           | "pendulum" -> (module Pendulum.Make (Sim_env) : Simulation.S)
           | "cartpole" -> (module Cartpole.Make (Sim_env) : Simulation.S)
           | _ -> failwith "Unknown simulation")
  end in
  (* Choose the algorithm modules *)
  let module Algo = struct
    include
      (val match Algo_config.algo_name with
           | "qlearning" ->
               (module Qlearning.Make (Algo_config) (Sim)
               : Base_algorithm.Algo_base)
           | _ -> failwith "Unknown algorithm")
  end in
  Algo.train ();
  Algo.save_model ()
