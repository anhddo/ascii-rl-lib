[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

module Make =
functor
  (C : Simulation.Config)
  ->
  struct


    include Simulation.T
    let env_type = Pendulum

    (* Returns the squre of the float *)
    let square (value : float) : float = Float.pow value 2.

    (* if pow don't work for you use the following *)
    (* let square (value : float) : float = value ** 2. *)

    (* Does floating point modulo *)
    let modulo (dividend : float) (divisor : float) : float =
      dividend -. (divisor *. floor (dividend /. divisor))

    (* Normalizes an angle to be between -PI and PI*)
    let normalize_angle (ang : float) : float =
      modulo (ang +. Float.pi) (2. *. Float.pi) |> Float.sub Float.pi

    (* Add These Tests And To .mli *)
    let random_between (min : float) (max : float) : float =
      let diff = max -. min in
      Random.float diff +. min

    let clip (min_value : 'a) (max_value : 'a) (value : 'a) : 'a =
      if value < min_value then min_value
      else if value > max_value then max_value
      else value

    (* type t = float list (* Length is 2 | [location, ang_speed ] *)
       type action = float list *)
    (* Length is 1 | [amount of torque to apply between -2 and 2] *)

    (* observation : the new state of the simulation; the next step call should use this value *)
    (* reward : maximum reward is 0, achieved when pendulum is perfectly balanced *)
    (* terminated : Not relevant, since simulation is everlasting and there are no such thing as episodes ??? *)
    (* truncated : idk *)
    (* info : error handling, nothing for now *)

    (* Creates a new simulation *)
    let create () : t = [ 0.0; 0.0 ]

    let convert_fo_feature (sim : t) : float list =
      let angle = List.nth sim 0 in
      let angular_speed = List.nth sim 1 in
      [ Stdlib.cos angle; Stdlib.sin angle; angular_speed ]

    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      (* TODO : possibly have the constants be held in the sim list *)
      let random_starting_angle = random_between (-1.0 *. Float.pi) Float.pi in
      let random_starting_angular_speed = random_between (-1.) 1. in
      let internal_state =
        [ random_starting_angle; random_starting_angular_speed; 0. ]
      in
      (convert_fo_feature internal_state, internal_state)

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      (* these constants can be changed to create variable environments *)
      let constant_timestep = 0.05 (* seconds *) in
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
          let applied_torque =
            clip (Float.neg max_torque) max_torque applied_torque
          in
          let reward =
            (* Penalizes high applied torque, high angular speeds, and deviation from the top position *)
            old_ang |> normalize_angle |> square
            |> Float.add @@ (0.1 *. square old_angspeed)
            |> Float.add @@ (0.001 *. square applied_torque)
            |> Float.mul (-1.)
          in
          let new_angspeed =
            (* TODO, EXPLAIN THE PHYSICS *)
            let gravity_angacceleration =
              3. *. gravity /. (2. *. length) *. Float.sin old_ang
            in
            let applied_angaccleration =
              3. /. (mass *. square length) *. applied_torque
            in
            old_angspeed
            |> Float.add
               @@ (gravity_angacceleration +. applied_angaccleration)
                  *. constant_timestep
            |> clip (Float.neg max_angspeed) max_angspeed
          in
          let new_ang =
            old_ang |> Float.add @@ (new_angspeed *. constant_timestep)
          in
          {
            observation = convert_fo_feature [ new_ang; new_angspeed ];
            reward;
            terminated = false;
            truncated = timestep > 200.;
            info = "";
            internal_state = [ new_ang; new_angspeed; timestep +. 1. ];
          }
      | _ -> failwith "Improper Input"

    let render (sim_state : t) : unit =
      (* prem *)
      let term_width = 80 in
      let term_height = 24 in
      let scale = 10.0 in

      match sim_state with
      | [ theta; _; _ ] ->
          let x = scale *. sin theta in
          let y = scale *. cos theta in

          (* project to col row *)
          let col = int_of_float (float_of_int (term_width / 2) +. x) in
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
          if row >= 0 && row < term_height && col >= 0 && col < term_width then
            canvas.(row).(col) <- 'O';

          (* clean screen *)
          print_string "\027[2J\027[H";
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
      Printf.printf "internal state: %s\n"
        (String.concat ", " (List.map Float.to_string response.internal_state));
      (* render response.internal_state; *)
      simulate response.internal_state
  end
