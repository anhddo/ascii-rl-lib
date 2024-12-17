module Make =
functor
  (C : Simulation.Config)
  ->
  struct
    include Simulation.T

    let env_type = Pendulum

    (* type t = float list Length is 2 | [ angle of pendulum from upright, angular velocity of pendulum ] *)
    (* type action = float list Length is 1 | [ amount of torque to apply ] *)

    (* Creates a new simulation *)
    let create () : t = [ 0.0; 0.0; 0.0 ]

    (* Translates state into a features for the algorithm *)
    let convert_to_feature (sim : t) : float list =
      let angle = List.nth sim 0 in
      let angular_speed = List.nth sim 1 in
      [ Stdlib.cos angle; Stdlib.sin angle; angular_speed ]

    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      (* clean screen *)
      if C.render then print_string "\027[2J\027[H";
      (* Generate Random Beginning State *)
      let random_starting_angle = Utils.random_between (-1.0 *. Float.pi) Float.pi in
      let random_starting_angular_speed = Utils.random_between (-1.) 1. in
      let internal_state =
        [ random_starting_angle; random_starting_angular_speed; 0. ]
      in
      (convert_to_feature internal_state, internal_state)

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      (* Create Constants for Environment *)
      let constant_timestep = 0.05 in
      let gravity = 10. in
      let mass = 1. in
      let length = 1. in
      let max_torque = 2. in
      let max_angspeed = 8. in
      match (sim, act) with
      | ( [
            old_ang (* radians from top *);
            old_angspeed (* radians per second *);
            timestep;
          ],
          applied_torque (* Newton-Meters *) :: [] ) ->
          let applied_torque = Utils.clip (Float.neg max_torque) max_torque applied_torque
          in
          if C.render then 
            begin
              Printf.printf "\027[1;1H";
              (* move cursor to top left*)
              Printf.printf "Applied torque:\t %f\n" applied_torque 
            end;
          let reward =
            (* Penalizes high applied torque, high angular speeds, and deviation from the top position *)
            old_ang |> Utils.normalize_angle |> Utils.square
            |> Float.add @@ (0.1 *. Utils.square old_angspeed)
            |> Float.add @@ (0.001 *. Utils.square applied_torque)
            |> Float.mul (-1.)
          in
          let new_angspeed =
            let gravity_angacceleration =
              3. *. gravity /. (2. *. length) *. Float.sin old_ang
            in
            let applied_angaccleration =
              3. /. (mass *. Utils.square length) *. applied_torque
            in
            old_angspeed
            |> Float.add @@ (gravity_angacceleration +. applied_angaccleration) *. constant_timestep
            |> Utils.clip (Float.neg max_angspeed) max_angspeed
          in
          let new_ang =
            old_ang 
            |> Float.add @@ (new_angspeed *. constant_timestep)
            |> Utils.normalize_angle
          in
          {
            observation = convert_to_feature [ new_ang; new_angspeed ];
            reward;
            terminated = false;
            truncated = timestep > 200.;
            info = "";
            internal_state = [ new_ang; new_angspeed; timestep +. 1. ];
          }
      | _ -> failwith "Improper Input"

    [@@@coverage off] (* turn off coverage for rendering *)
    let render (sim_state : t) : unit =
      if C.render = true then
        let term_width = 80 in
        let term_height = 24 in
        let scale = 10.0 in

        match sim_state with
        | [ theta; angle_speed; time_step ] ->
            let x = scale *. sin theta in
            let y = scale *. cos theta in

            (* project to col row *)
            let col = int_of_float (float_of_int (term_width / 2) +. x/. 0.5) in
            let row = int_of_float (float_of_int (term_height / 2) -. y) in

            (* init canvas *)
            let canvas = Array.make_matrix term_height term_width ' ' in

            (* draw pivot *)
            let pivot_row = term_height / 2 in
            let pivot_col = term_width / 2 in
            canvas.(pivot_row).(pivot_col) <- '+';

            (* draw line *)
            let draw_line x0 y0 x1 y1 =
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
                    then (
                      canvas.(yi).(xi) <- '.';
                      if xi + 1 < term_width then canvas.(yi).(xi + 1) <- '.');
                    loop (i + 1) (x +. x_increment) (y +. y_increment))
                in
                loop 0 (float x0) (float y0)
            in

            (* draw pivot *)
            draw_line pivot_col pivot_row col row;

            (* draw bottom *)
            if row >= 0 && row < term_height && col >= 0 && col < term_width
            then canvas.(row).(col) <- 'O';

            (* clean screen *)
            (* print_string "\027[2J\027[H"; *)
            Printf.printf "\027[2;1H";
            (* move cursor to top left, this trick avoid flickering *)
            Printf.printf "Angle:\t\t%f\nAngular speed:\t%f\nTime step:\t%d\n" theta
              angle_speed (int_of_float time_step);
            Array.iter
              (fun row ->
                Array.iter print_char row;
                print_newline ())
              canvas;

            (* delay *)
            Unix.sleepf 0.05
        | _ -> failwith "Invalid simulation state"

    let rec simulate sim_state =
      let action = [ 0. ] in
      let response = step sim_state action in
      render response.internal_state;
      simulate response.internal_state
    [@@@coverage on]
  end
