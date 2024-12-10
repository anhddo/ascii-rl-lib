[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

module Make =
functor
  (C : Simulation.Config)
  ->
  struct


    include Simulation.T
    let env_type = Cartpole

    let square (value : float) : float = Float.pow value 2.

    let random_between (min : float) (max : float) : float =
      let diff = max -. min in
      Random.float diff +. min

    (* type t = float list Length is 5 | [ location of cart, velocity of cart, angle of pole, angular velocity of pole, steps_past_terminated ] *)
    (* type action = float list Length is 1 | [ push cart left (0.) or right (1.)] *)

    (* Creates a new simulation *)
    let create () : t = [0.; 0.; 0.; 0.; 0.]

    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      let min_start = -0.05 in
      let max_start = 0.05 in 
      let starting_state = [
        random_between min_start max_start;
        random_between min_start max_start;
        random_between min_start max_start;
        random_between min_start max_start;
        -1.
      ]
      in
      (starting_state, starting_state);; 

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      let gravity = 9.8 in 
      let cart_mass = 1. in 
      let pole_mass = 0.1 in 
      let total_mass = cart_mass +. pole_mass in 
      let pole_length = 0.5 in 
      let pole_moment = pole_mass *. pole_length in 
      let force_magnitutde = 10. in
      let timestep = 0.2 in 
      let pole_angle_threshold = 12. *. 2. *. Float.pi /. 360. in 
      let cart_location_threshold = 2.4 in
      let force = 
        match act with 
        | 0. :: [] -> -1. *. force_magnitutde 
        | 1. :: [] -> force_magnitutde  
        | _ -> failwith "invalid action"
      in
      let cart_location, cart_velocity, pole_angle, pole_ang_velocity, steps_past_terminated = 
        match sim with 
        | [cart_location; cart_velocity; pole_angle; pole_ang_velocity; steps_past_terminated] -> (cart_location, cart_velocity, pole_angle, pole_ang_velocity, steps_past_terminated)
        | _ -> failwith "invalid state"
      in
      let sintheta = Float.sin pole_angle 
      in
      let costheta = Float.cos pole_angle
      in
      let x_acceleration_partial = (force +. pole_moment *. (square pole_ang_velocity) *. sintheta) /. total_mass
      in
      let denominator = pole_length *. ((4.0 /. 3.0) -. (pole_mass *. (square costheta) /. total_mass))
      in
      let pole_angular_acceleration = ((gravity *. sintheta) -. (costheta *. x_acceleration_partial)) /. denominator
      in
      let x_acceleration_complete = x_acceleration_partial -. (pole_moment *. pole_angular_acceleration *. costheta /. total_mass)
      in
      (* Update using Euler method *)
      let new_cart_location = cart_location +. timestep *. cart_velocity 
      in
      let new_cart_velocity = cart_velocity +. timestep *. x_acceleration_complete
      in 
      let new_pole_angle = pole_angle +. timestep *. pole_ang_velocity
      in
      let new_pole_ang_velocity = pole_ang_velocity +. timestep *. pole_angular_acceleration
      in
      let terminated = 
        match (Float.abs (new_cart_location) > cart_location_threshold, Float.abs (new_pole_angle) > pole_angle_threshold) with 
        | true, _ | _, true -> true
        | _, _ -> false
      in
      let reward, steps_past_terminated = 
        match (terminated, steps_past_terminated >= 0.) with 
        | false, _ -> (1.0, -1.)
        | true, true -> (0., steps_past_terminated +. 1.)
        | true, false -> (1., 0.)
      in
      let new_state = [
        new_cart_location;
        new_cart_velocity;
        new_pole_angle;
        new_pole_ang_velocity;
        steps_past_terminated
      ]
      in
      {
        observation = new_state;
        reward = reward;
        terminated = terminated;
        truncated = false;
        info = "";
        internal_state = new_state;
      }

    let render (sim_state : t) : unit =
      failwith "todo"

  end
