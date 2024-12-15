module type Config = sig
  val simulation_name : string
  val render : bool
end

module type T = sig
  type env_type =
    | Cartpole
    | Pendulum
    | Blackjack
    | Gym_env

  (* Should store all the information to uniquely identify any possible simulation state *)
  type t = float list

  (* Can change based on what the simulation needs *)
  type action = float list

  type response = {
    observation : t; (* The observation of the simulation given to the reinforcement learning algorithms *)
    reward : float; (* The reward given to the reinforcement learning algorithm *)
    terminated : bool; (* If the natural state of the simulation calls for an end *)
    truncated : bool; (* If the simulation ends for another reason, such as a time limit *)
    info : string; (* Any additional information *)
    internal_state : t; (* The observation of the simulation given back to the step function *)
  }
end

module type S = sig
  (* Simulation Type *)
  (* include Env_type.S *)
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

module T : T = struct
  type env_type =
    | Cartpole
    | Pendulum
    | Blackjack
    | Gym_env

  type t = float list
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
