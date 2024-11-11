(* [@@@ocaml.warning "-27"]

module Pendulum : Simulation.S = struct
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
    [ random_starting_angle; random_starting_angular_speed ], "Simulation Begun"
    (* {
      observation = [ random_starting_angle; random_starting_angular_speed ];
      reward = 0.;
      terminated = false;
      truncated = false;
      info = "Simulation Begun";
    } *)

  let step (act : action) : response = 
    failwith "todo"
  let render (sim : t) : char list = failwith "todo"
end

(*
Theoretically, after we finish implementing one of these, the reinforcement learning models can start training
- pendulum and likely all others have full documentation on how these work

We can then get to work on making a UI to these, or making more


Blackjack, Pendulum, Cart Pole

*) *)


module Pendulum : Simulation.T = 
  sig
  	
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
    let reset (sim : t) : response =
      let random_starting_angle = Random.float (2. *. Float.pi) -. Float.pi in
      let random_starting_angular_speed = Random.float 2. -. 1. in
      {
        observation = [ random_starting_angle; random_starting_angular_speed ];
        reward = 0.;
        terminated = false;
        truncated = false;
        info = "Simulation Begun";
      };;

    (* Applies the action to the environment, and returns the corresponding response *)
    let step : (sim : t) (act : action) : response =
      failwith "TODO";;

    (* Take a simulation and render into a viewable format *)
    let render (sim : t) : char list = (* char list is temporary idea, can and may likely change *)
      failwith "TODO";;
  end
