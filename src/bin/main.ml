[@@@ocaml.warning "-32"]

type config = {
  episode : int;
  model_path : string;
  render : bool;
  simulation : string;
  algo : string;
}

(* Default configuration *)
let default_config =
  {
    episode = 0;
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
    | "--model-path" :: value :: rest ->
        update_config { config with model_path = value } rest
    | "--simulation" :: value :: rest ->
        update_config { config with simulation = value } rest
    | "--algo" :: value :: rest ->
        update_config { config with algo = value } rest
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
    let learning_rate = 0.1
  end in
  let module Sim = struct
    include
      (val match Sim_env.simulation_name with
           | "pendulum" -> (module Pendulum.Make (Sim_env) : Simulation.S)
           | "cartpole" -> (module Cartpole.Make (Sim_env) : Simulation.S)
           | "blackjack" -> (module Blackjack.Make (Sim_env) : Simulation.S)
           | _ -> failwith "Unknown simulation")
  end in
  let module Algo = struct
    include
      (val match Algo_config.algo_name with
           | "qlearning" ->
               (module Qlearning.Make (Algo_config) (Sim)
               : Base_algorithm.Algo_base)
           | "vpg" ->
               (module Vpg.Make (Algo_config) (Sim) : Base_algorithm.Algo_base)
           | "vpgnn" ->
               (module Vpgnn.Make (Algo_config) (Sim) : Base_algorithm.Algo_base)
           | _ -> failwith "Unknown algorithm")
  end in
  Algo.train config.episode;
  Algo.save_model ()
