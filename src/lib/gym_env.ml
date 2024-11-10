[@@@ocaml.warning "-27"]

type t = float list (* Length is 2 | [location, ang_speed ] *)

type action =
  float list (* Length is 1 | [amount of torque to apply between -2 and 2] *)

(* observation : the new state of the simulation; the next step call should use this value *)
(* reward : maximum reward is 0, achieved when pendulum is perfectly balanced *)
(* terminated : idk, error handling? *)
(* truncated : idk *)
(* info : error handling, nothing for now *)
type response = {
  observation : t;
  reward : float;
  terminated : bool;
  truncated : bool;
  info : string;
}

let create () : t = [ 0.; 0. ]

let reset (sim : t) : t * string =
  let random_starting_angle = Random.float (2. *. Float.pi) -. Float.pi in
  let random_starting_angular_speed = Random.float 2. -. 1. in
  ([ random_starting_angle; random_starting_angular_speed ], "Simulation Begun")
(* {
     observation = [ random_starting_angle; random_starting_angular_speed ];
     reward = 0.;
     terminated = false;
     truncated = false;
     info = "Simulation Begun";
   } *)

let step (sim : t) (act : action) : response = failwith "todo"
let render (sim : t) : char list = failwith "todo"

(*
Theoretically, after we finish implementing one of these, the reinforcement learning models can start training
- pendulum and likely all others have full documentation on how these work

We can then get to work on making a UI to these, or making more

*)
