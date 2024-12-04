[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

module Make =
functor
  (C : Simulation.Config)
  ->
  struct


    include Simulation.T
    let env_type = Cartpole

    let random_between (min : float) (max : float) : float =
      let diff = max -. min in
      Random.float diff +. min

    (* type t = float list Length is 4 | [ location of cart, velocity of cart, angle of pole, angular velocity of pole ] *)
    (* type action = float list Length is 1 | [ push cart left or right ] *)

    (* Creates a new simulation *)
    let create () : t = [0.; 0.; 0.; 0.]

    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      let min_start = -0.05 in
      let max_start = 0.05 in 
      let starting_state = [
        random_between min_start max_start;
        random_between min_start max_start;
        random_between min_start max_start;
        random_between min_start max_start;
      ]
      in
      (starting_state, starting_state);; 

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      failwith "todo"

    let render (sim_state : t) : unit =
      failwith "todo"

  end
