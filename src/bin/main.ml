(* let episode = ref 1
   let algo = ref "qlearning"
   let env = ref "cartpole" *)
(* module Config = struct
     let name = !env
     let render = false
   end
   module Env1 = Gym_env.Make (Config) *)

(* let main =
   let speclist =
     [
       ( "-episode",
         Arg.Int (fun n -> episode := n),
         "number of episodes to train" );
       ("-algo", Arg.String (fun s -> algo := s), "the algorithm to use");
       ("-env", Arg.String (fun s -> env := s), "the environment to use");
     ]
   in
   Arg.parse speclist print_endline "";
   print_int !episode;

   Qlearning.train !episode *)

(* open Core *)

(* module Config : Simulation.Config = struct
  include Simulation.Config

  (* let q_table_path = "cartpole.sexp" *)
  let name = "Pendulum-v1"
  let q_table_path = "pendulum.sexp"
end

module Config_console : Simulation.Config = struct
  include Simulation.Config

  (* let q_table_path = "cartpole.sexp" *)
  let name = "Pendulum-v1"
  let q_table_path = "console.sexp"
end
module Config_console_render : Simulation.Config = struct
  include Config_console

  (* let q_table_path = "cartpole.sexp" *)
  let render = true
end

module Config_render : Simulation.Config = struct
  include Config
  let render = true
end *)

(* module QLearning1 = Qlearning.Make (Config)
module QLearning2 = Qlearning.Make (Config_render) *)
(* module QLearning3 = Qlearning.Make (Pendulum) *)
(* module QLearning3 = Qlearning.Make (Config_console)
module QLearning4 = Qlearning.Make (Config_console_render) *)


let () =
  (* let s =
       Sys.get_argv () |> Array.to_list
       |> List.fold ~init:"" ~f:(fun acc a -> acc ^ a)
     in
     print_string s; *)
  (* QLearning1.train 3000;
  QLearning1.save_q_table ();
  QLearning2.train 2; *)
  Printf.printf "Pendulum\n";

  (* QLearning3.train 30000;
  QLearning3.save_q_table ();
  QLearning4.train 7; *)
  (* QLearning3.train 30; *)
