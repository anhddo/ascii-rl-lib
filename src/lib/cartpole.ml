[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

module Make =
functor
  (C : Simulation.Config)
  ->
  struct


    include Simulation.T
    let env_type = Cartpole


    (*TODO type t = float list (* Length is 2 | [location, ang_speed ] *)
       type action = float list *)
    (* TODO Length is 1 | [amount of torque to apply between -2 and 2] *)

    (* observation : the new state of the simulation; the next step call should use this value *)
    (* reward : maximum reward is 0, achieved when pendulum is perfectly balanced *)
    (* terminated : Not relevant, since simulation is everlasting and there are no such thing as episodes ??? *)
    (* truncated : idk *)
    (* info : error handling, nothing for now *)

    (* Creates a new simulation *)
    let create () : t = 
      failwith "todo"

    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      failwith "todo"

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      failwith "todo"

    let render (sim_state : t) : unit =
      failwith "todo"

    let rec simulate sim_state =
      let action = [ 0. ] in
      let response = step sim_state action in
      Printf.printf "internal state: %s\n"
        (String.concat ", " (List.map Float.to_string response.internal_state));
      (* render response.internal_state; *)
      simulate response.internal_state
  end
