

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

module Make =
functor
  (C : Simulation.Config)
  ->
  struct
    include Simulation.T

    let env_type = LunarLander

    (* type t = float list (* Length is ? | [ ?? ] *)
       type action = float list *)
    (* Length is ? | [??] *)

    (* Creates a new simulation *)
    let create () : t = [ 0.0; 0.0 ]

    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      failwith "todo"

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      failwith "todo"

    let render (sim_state : t) : unit =
      failwith "todo"
  end
