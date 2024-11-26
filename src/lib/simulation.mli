module type T = sig
  (* Simulation Type *)
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

  (* Creates a new simulation *)
  val create : unit -> t

  (* Resets the simulation and returns the first response again *)
  val reset : unit -> t * t

  (* Applies the action to the environment, and returns the corresponding response *)
  val step : t -> action -> response

  (* Take a simulation and render into a viewable format *)
  val render : t -> unit
end

module type Config = sig
  (* type t = { name : string; render : bool } *)
  val name : string
  val render : bool
  val q_table_path : string
end

module Config : Config
