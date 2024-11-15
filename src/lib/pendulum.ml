[@@@ocaml.warning "-27"]

(*
Theoretically, after we finish implementing one of these, the reinforcement learning models can start training
- pendulum and likely all others have full documentation on how these work

We can then get to work on making a UI to these, or making more


Blackjack, Pendulum, Cart Pole

*)


(* Returns the squre of the float *)
let square (value : float) : float = 
  Float.pow value 2.;;

(* Does floating point modulo *)
let modulo (dividend : float) (divisor : float) : float = 
  dividend -. (divisor *. floor(dividend /. divisor));;

(* Normalizes an angle to be between -PI and PI*)
let normalize_angle (ang : float) : float = 
  modulo (ang +. Float.pi) (2. *. Float.pi) 
  |> Float.sub Float.pi;;

(* Add These Tests And To .mli *)
let random_between (min : float) (max : float) : float =
  let diff = max -. min in 
  Random.float(diff) +. min;;

let clip (min_value : 'a) (max_value : 'a) (value : 'a) : 'a =
  if (value < min_value) then min_value
  else if (value > max_value) then max_value
  else value;;

type t = float list (* Length is 2 | [location, ang_speed ] *)

type action = float list (* Length is 1 | [amount of torque to apply between -2 and 2] *)

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

(* Creates a new simulation *)
let create() : t = 
  [0.0; 0.0];;


(* Resets the simulation and returns the first response again *) 
let reset (sim : t) : response = (* TODO : possibly have the constants be held in the sim list *)
  let random_starting_angle = random_between (-1.0 *. Float.pi) (Float.pi) in
  let random_starting_angular_speed = random_between (-1.) (1.) in
  {
    observation = [ random_starting_angle; random_starting_angular_speed ];
    reward = 0.;
    terminated = false;
    truncated = false;
    info = "Simulation Begun";
  };;

(* Applies the action to the environment, and returns the corresponding response *)
let step (sim : t) (act : action) : response =
  (* these constants can be changed to create variable environments *)
  let constant_timestep = 0.05 (* seconds *) 
  in
  let gravity = 10. 
  in 
  let mass = 1. 
  in 
  let length = 1. 
  in 
  let max_torque = 2. 
  in
  let max_angspeed = 8. 
  in
  match (sim, act) with 
  | old_ang (* radians from top *) :: old_angspeed (* radians per second *):: [],
    applied_torque (* Newton-Meters *) :: [] -> 
      let applied_torque = clip (Float.neg max_torque) max_torque applied_torque
      in
      let reward = (* Penalizes high applied torque, high angular speeds, and deviation from the top position *)
        old_ang 
        |> normalize_angle 
        |> square
        |> Float.add @@ 0.1 *. (square old_angspeed)
        |> Float.add @@ 0.001 *. (square applied_torque)
        |> Float.mul (-1.)
      in 
      let new_angspeed = (* TODO, EXPLAIN THE PHYSICS *)
        let gravity_angacceleration  = (3. *. gravity) /. (2. *. length) *. (Float.sin old_ang)
        in
        let applied_angaccleration = 3. /. (mass *. (square length)) *. applied_torque
        in
        old_angspeed 
        |> Float.add @@ (gravity_angacceleration +. applied_angaccleration) *. constant_timestep
        |> clip (Float.neg max_angspeed) max_angspeed
      in
      let new_ang = 
        old_ang
        |> Float.add @@ new_angspeed *. constant_timestep
      in
      {
        observation = [new_ang; new_angspeed];
        reward = reward;
        terminated = false;
        truncated = false;
        info = ""
      } 
  | _ -> failwith "Improper Input"

(* Take a simulation and render into a viewable format *)
let render (sim : t) : char list = (* char list is temporary idea, can and may likely change *)
  failwith "TODO";;
