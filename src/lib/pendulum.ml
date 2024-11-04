module type simulation =
  sig
  	(* Simulation Type *)
  	type t = float list (* Should store all the information to uniquely identify any possible simulation state *)

  	(* Can change based on what the simulation needs *)
    type action = int
    type response = { observation : float list; reward : float; terminated : bool; truncated : bool; info : string }

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
    type t = {location : float; ang_speed : float}
    type action = int (* amount of torque to apply *)
    
    (* observation : [clockwise angle from top in radians, angular speed of bar in radians per step]*)
    (* reward : to figure out; it is documented formula *)
    (* terminated : if finished at top stably with speed 0 *)
    (* truncated : idk *)
    (* info : error handling, nothing for now *)
    type response = { observation : t; reward : float ; terminated : bool; truncated : bool; info : string }

    let create : t = 
      {location = 0; ang_speed = 0};;

    let reset (sim : t) : response = 
      failwith "todo";;

    let step (sim : t) (act : action) : response = 
      failwith "todo";;
end


(*
Theoretically, after we finish implementing one of these, the reinforcement learning models can start training
- pendulum and likely all others have full documentation on how these work

We can then get to work on making a UI to these, or making more

*)