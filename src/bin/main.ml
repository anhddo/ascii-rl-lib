(* Variables to store the parsed values *)
let episode = ref 0
let model_path = ref ""
let render = ref false

(* Function to set the episode *)

(* Command-line argument specification *)
let speclist =
  [
    ("--episode", Arg.Set_int episode, "Set the number of episodes (integer)");
    ( "--model-path",
      Arg.Set_string model_path,
      "Set the path to the model (string)" );
    ( "--render",
      Arg.Set render,
      "Enable rendering (boolean flag, default: false)" );
    (*We will include the following argument after we integrate other simulations and algorithms
      "--algo", Arg.Set algo, "algorithm name" );
      "--simulation", Arg.Set Simulation, "algorithm name" ); *)
  ]

(* Main program *)
let run (episode : int) (model_path : string) (render : bool) =
  let module Algo_config = struct
    let model_path = model_path
  end in
  let module Pendulum_env = Pendulum.Make (struct
    let render = render
  end) in
  let module Qlearning_algo = Qlearning.Make (Algo_config) (Pendulum_env) in
  Qlearning_algo.train episode;
  Qlearning_algo.save_model ()

(* Entry point *)
let () =
  Arg.parse speclist print_endline "";

  (* Validate required arguments *)
  run !episode !model_path !render
