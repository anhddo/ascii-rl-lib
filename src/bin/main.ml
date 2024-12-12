open Base_algorithm

(* Variables to store the parsed values *)
let episode = ref 0
let model_path = ref ""
let render = ref false
let algo = ref ""

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
    ("--algo", Arg.Set_string algo, "Algorithm name (e.g., qlearning, vpn)");
    (*We will include the following argument after we integrate other simulations and algorithms
      "--algo", Arg.Set algo, "algorithm name" );
      "--simulation", Arg.Set Simulation, "algorithm name" ); *)
  ]

(* Main program *)
let run (algo_name : string) (episode : int) (model_path : string) (render : bool) =
  let module Algo_config = struct
    let model_path = model_path
  end in
  let module Pendulum_env = Pendulum.Make (struct
    let render = render
  end) in

  (* 使用 match 动态选择算法模块 *)
  let module RL_algo = (val (
    match algo_name with
    | "qlearning" -> (module Qlearning.Make (Algo_config) (Pendulum_env) : Algo_base)
    | "vpg" -> (module Vpg.Make (Algo_config) (Pendulum_env) : Algo_base)
    (* | "vpgnn" -> (module Vpgnn.Make (Algo_config) (Pendulum_env) : Algo_base) *)
    | _ ->
        Printf.eprintf "Error: Unsupported algorithm '%s'.\n" algo_name;
        exit 1
  )) in

  (* 调用选定模块的方法 *)
  RL_algo.train episode;
  RL_algo.save_model ()


(* Entry point *)
let () =
  Arg.parse speclist print_endline "";

  (* Validate required arguments *)
  run !algo !episode !model_path !render