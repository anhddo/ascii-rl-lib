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

module Config = struct
  let name = "Pendulum-v1"
  (* let name = "CartPole-v1" *)
  let render = false
end

module QLearning1 = Qlearning.Make (Config)

let () =
  (* let s =
    Sys.get_argv () |> Array.to_list
    |> List.fold ~init:"" ~f:(fun acc a -> acc ^ a)
  in
  print_string s; *)
  QLearning1.train 1000
  

