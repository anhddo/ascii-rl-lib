module type Config = sig
  (* type t = { name : string; render : bool } *)
  val render : bool
end

module type T = sig
  type env_type =
    | Cartpole
    | Pendulum
    | LunarLander
    | Blackjack

  type t = float list
  (* Should store all the information to uniquely identify any possible simulation state *)

  (* Can change based on what the simulation needs *)
  type action = float list

  type response = {
    observation : t;
    reward : float;
    terminated : bool;
    truncated : bool;
    info : string;
    internal_state : t;
  }
end

module type S = sig
  (* Simulation Type *)
  include T

  val env_type : env_type

  (* Creates a new simulation *)
  val create : unit -> t

  (* Resets the simulation and returns the first response again *)
  val reset : unit -> t * t

  (* Applies the action to the environment, and returns the corresponding response *)
  val step : t -> action -> response

  (* Take a simulation and render into a viewable format *)
  val render : t -> unit
end

module T : T
