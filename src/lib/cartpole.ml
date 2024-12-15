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
    let create () : t = [ 0.; 0.; 0.; 0.; 0. ]

    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      let min_start = -0.05 in
      let max_start = 0.05 in
      let starting_state =
        [
          random_between min_start max_start;
          random_between min_start max_start;
          random_between min_start max_start;
          random_between min_start max_start;
          -1.;
        ]
      in
      (starting_state, starting_state)

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      let gravity = 9.8 in
      let cart_mass = 1. in
      let pole_mass = 0.1 in
      let total_mass = cart_mass +. pole_mass in
      let pole_length = 0.5 in
      let pole_moment = pole_mass *. pole_length in
      let force_magnitutde = 10. in
      let timestep = 0.05 in
      let pole_angle_threshold = 12. *. 2. *. Float.pi /. 360. in
      let cart_location_threshold = 2.4 in
      let force =
        match act with
        | 0. :: [] -> -1. *. force_magnitutde
        | 1. :: [] -> force_magnitutde
        | _ -> failwith "invalid action"
      in
      let ( cart_location,
            cart_velocity,
            pole_angle,
            pole_ang_velocity,
            steps_past_terminated ) =
        match sim with
        | [
         cart_location;
         cart_velocity;
         pole_angle;
         pole_ang_velocity;
         steps_past_terminated;
        ] ->
            ( cart_location,
              cart_velocity,
              pole_angle,
              pole_ang_velocity,
              steps_past_terminated )
        | _ -> failwith "invalid state"
      in
      let sintheta = Float.sin pole_angle in
      let costheta = Float.cos pole_angle in
      let x_acceleration_partial =
        (force +. (pole_moment *. square pole_ang_velocity *. sintheta))
        /. total_mass
      in
      let denominator =
        pole_length
        *. ((4.0 /. 3.0) -. (pole_mass *. square costheta /. total_mass))
      in
      let pole_angular_acceleration =
        ((gravity *. sintheta) -. (costheta *. x_acceleration_partial))
        /. denominator
      in
      let x_acceleration_complete =
        x_acceleration_partial
        -. (pole_moment *. pole_angular_acceleration *. costheta /. total_mass)
      in
      (* Update using Euler method *)
      let new_cart_location = cart_location +. (timestep *. cart_velocity) in
      let new_cart_velocity =
        cart_velocity +. (timestep *. x_acceleration_complete)
      in
      let new_pole_angle = pole_angle +. (timestep *. pole_ang_velocity) in
      let new_pole_ang_velocity =
        pole_ang_velocity +. (timestep *. pole_angular_acceleration)
      in
      let terminated =
        match
          ( Float.abs new_cart_location > cart_location_threshold,
            Float.abs new_pole_angle > pole_angle_threshold )
        with
        | true, _ | _, true -> true
        | _, _ -> false
      in
      let reward, steps_past_terminated =
        match (terminated, steps_past_terminated >= 0.) with
        | false, _ -> (1.0, -1.)
        | true, true -> (0., steps_past_terminated +. 1.)
        | true, false -> (1., 0.)
      in
      let new_state =
        [
          new_cart_location;
          new_cart_velocity;
          new_pole_angle;
          new_pole_ang_velocity;
          steps_past_terminated;
        ]
      in
      {
        observation = new_state;
        reward;
        terminated;
        truncated = false;
        info = "";
        internal_state = new_state;
      }

    let render (sim_state : t) : unit =
      if C.render then
        let term_width = 80 in
        let term_height = 24 in
        let pivot_row = term_height / 2 in
        let track_min = -2.4 in
        let track_max = 2.4 in
        let track_range = track_max -. track_min in
        let scale_x = float_of_int term_width /. track_range in

        match sim_state with
        | [
         cart_x;
         cart_velocity;
         pole_angle;
         pole_ang_velocity;
         steps_past_terminated;
        ] ->
            Printf.printf "\027[2J\027[H";
            let canvas = Array.make_matrix term_height term_width ' ' in
            for i = 0 to term_width - 1 do
              canvas.(pivot_row + 1).(i) <- '-'
            done;

            let cart_col = int_of_float ((cart_x -. track_min) *. scale_x) in
            let cart_str = "[===]" in
            let cart_len = String.length cart_str in
            let cart_start =
              max 0 (min (term_width - cart_len) (cart_col - (cart_len / 2)))
            in

            for i = 0 to cart_len - 1 do
              canvas.(pivot_row).(cart_start + i) <- cart_str.[i]
            done;

            let pole_length = 1.5 in
            let scale_pole = 5.0 in

            let x0 = cart_start + (cart_len / 2) in
            let y0 = pivot_row in
            let pole_x_top =
              float_of_int x0 +. (sin pole_angle *. scale_pole *. pole_length)
            in
            let pole_y_top =
              float_of_int y0 -. (cos pole_angle *. scale_pole *. pole_length)
            in

            let x1 = int_of_float pole_x_top in
            let y1 = int_of_float pole_y_top in

            let pole_char =
              if pole_angle > 0.1 then '/'
              else if pole_angle < -0.1 then '\\'
              else '.'
            in

            let draw_line_with_char x0 y0 x1 y1 ch_end ch_mid =
              let dx = x1 - x0 in
              let dy = y1 - y0 in
              let steps = max (abs dx) (abs dy) in
              if steps > 0 then
                let x_increment = float dx /. float steps in
                let y_increment = float dy /. float steps in
                let rec loop i x y =
                  if i <= steps then (
                    let xi = int_of_float x in
                    let yi = int_of_float y in
                    if xi >= 0 && xi < term_width && yi >= 0 && yi < term_height
                    then
                      if i = steps then canvas.(yi).(xi) <- ch_end
                      else canvas.(yi).(xi) <- ch_mid;
                    loop (i + 1) (x +. x_increment) (y +. y_increment))
                  else ()
                in
                loop 0 (float x0) (float y0)
            in
            draw_line_with_char x0 y0 x1 y1 'O' pole_char;
 
            Printf.printf
              "Cart Position: %f\n\
               Cart Velocity: %f\n\
               Pole Angle (rad): %f\n\
               Pole Angular Velocity: %f\n\
               Steps Past Terminated: %f\n"
              cart_x cart_velocity pole_angle pole_ang_velocity
              steps_past_terminated;

            Array.iter
              (fun row ->
                Array.iter print_char row;
                print_newline ())
              canvas;

            flush stdout
        | _ -> failwith "Invalid simulation state"

    let rec simulate (sim_state : t) =
      if C.render then Unix.sleepf 0.1;
      let action = [ (if Random.bool () then 1. else 0.) ] in
      let response = step sim_state action in
      render response.internal_state;
      if not response.terminated then simulate response.internal_state
      else Printf.printf "Episode terminated. Reward: %f\n" response.reward
  end
