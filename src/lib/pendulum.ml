[@@@warning "-27"]
[@@@warning "-69"]
module type simulation =
  sig
  	(* Simulation Type *)
  	type t = float list (* Should store all the information to uniquely identify any possible simulation state *)

  	(* Can change based on what the simulation needs *)
    type action = float list 
    type response = { observation : t; reward : float; terminated : bool; truncated : bool; info : string }

    (* Creates a new simulation *)
    val create : unit -> t 

    (* Resets the simulation and returns the first response again *)
    val reset : t -> response

    (* Applies the action to the environment, and returns the corresponding response *)
    val step : t -> action -> response

    (* Take a simulation and render into a viewable format *)
    val render : t -> char list (* char list is temporary idea, can and may likely change *)
  end

module Pendulum : simulation = 
  struct 
    type t = float list (* Length is 2 | [location, ang_speed ] *)
    type action = float list (* Length is 1 | [amount of torque to apply between -2 and 2] *)
    
    (* observation : the new state of the simulation; the next step call should use this value *)
    (* reward : maximum reward is 0, achieved when pendulum is perfectly balanced *)
    (* terminated : idk, error handling? *)
    (* truncated : idk *)
    (* info : error handling, nothing for now *)
    type response = { observation : t; reward : float ; terminated : bool; truncated : bool; info : string }

    let create : t = 
      {location = 0; ang_speed = 0};;

    let reset (sim : t) : response = 
      let random_starting_angle = Random.float (2 * Float.pi) @@ - Float.pi 
      in
      let random_starting_angular_speed = Random.float 2 @@ - 1
      in
      { observation = [random_starting_angle; random_starting_angular_speed]; reward = 0; terminated = false; truncated = false; info = "Simulation Begun" };;

    let step (sim : t) (act : action) : response =
      failwith "todo";;

    let render (sim : t) : char list = 
      failwith "todo";;
end


(*
Theoretically, after we finish implementing one of these, the reinforcement learning models can start training
- pendulum and likely all others have full documentation on how these work

We can then get to work on making a UI to these, or making more

*)