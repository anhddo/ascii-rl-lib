[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

module Make =
functor
  (C : Simulation.Config)
  ->
  struct


    include Simulation.T
    let env_type = Cartpole


    (*TODO type t = float list (* Length is ? | [ ?? ] *)
       type action = float list *)
    (* TODO Length is ? | [ ?? ] *)

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
