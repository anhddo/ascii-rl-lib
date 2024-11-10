(* Physics parameters for the cart-pole system *)
let gravity = 9.8           (* Gravity (m/s^2) *)
let mass_pole = 0.1         (* Mass of the pole (kg) *)
let mass_cart = 1.0         (* Mass of the cart (kg) *)
let length = 0.5            (* Half the length of the pole (m) *)
let time_step = 0.02        (* Time step for simulation (s) *)
let max_position = 3.0      (* Maximum position for back-and-forth motion *)

(* State variables for the cart-pole system *)
type state = {
  position : float;           (* Position of the cart *)
  velocity : float;           (* Velocity of the cart *)
  angle : float;              (* Angle of the pole from vertical (radians) *)
  angular_velocity : float;   (* Angular velocity of the pole *)
}

(* Update function for cart-pole physics based on current state and applied force *)
let update_state state force =
  let total_mass = mass_cart +. mass_pole in
  let pole_mass_length = mass_pole *. length in
  let cos_theta = cos state.angle in
  let sin_theta = sin state.angle in

  (* Calculate the equations of motion *)
  let temp = (force +. pole_mass_length *. state.angular_velocity ** 2. *. sin_theta) /. total_mass in
  let angular_acceleration = 
    (gravity *. sin_theta -. cos_theta *. temp) /. 
    (length *. (4.0 /. 3.0 -. mass_pole *. cos_theta ** 2. /. total_mass)) 
  in
  let linear_acceleration = temp -. pole_mass_length *. angular_acceleration *. cos_theta /. total_mass in

  (* Update the state *)
  {
    position = state.position +. time_step *. state.velocity;
    velocity = state.velocity +. time_step *. linear_acceleration;
    angle = state.angle +. time_step *. state.angular_velocity;
    angular_velocity = state.angular_velocity +. time_step *. angular_acceleration;
  }

(* Simple PD controller with a back-and-forth motion within a range *)
let control state direction =
  let k_p = -10.0 in          (* Proportional gain for angle *)
  let k_d = -1.0 in           (* Derivative gain for angular velocity *)
  let balancing_force = (k_p *. state.angle) +. (k_d *. state.angular_velocity) in

  (* Add a force to move the cart back and forth *)
  let movement_force = 
    if direction = "right" then 1.0
    else -1.0
  in

  balancing_force +. (movement_force *. 10.0)  (* Adjusting factor for smooth motion *)

(* Function to clear the screen *)
let clear_screen () =
  Printf.printf "\x1b[2J";
  flush stdout

(* Function to move the cursor to a specific position *)
let move_cursor row col =
  Printf.printf "\x1b[%d;%dH" row col;
  flush stdout

(* Function to draw the cart and pole based on the current state *)
let draw_cart_pole state =
  let cart_pos = int_of_float (state.position *. 10.0) + 40 in   (* Scale and center *)
  let pole_length = 10 in                                         (* Length of pole in characters *)

  (* Draw the cart *)
  move_cursor 10 cart_pos;
  Printf.printf "[]";  (* The cart *)
  
  (* Draw the pole *)
  for i = 0 to pole_length - 1 do
    let pole_x = cart_pos + int_of_float (float i *. sin state.angle) in
    let pole_y = 9 - int_of_float (float i *. cos state.angle) in
    move_cursor pole_y pole_x;
    Printf.printf "|"
  done;

  flush stdout

(* Main loop to run the cart-pole simulation *)
let simulate_cart_pole steps =
  let rec loop state n direction =
    if n <= 0 then ()
    else
      let force = control state direction in            (* Calculate the control force *)
      let new_state = update_state state force in       (* Update the state based on the force *)

      (* Change direction if reaching max_position bounds *)
      let new_direction = 
        if new_state.position > max_position then "left"
        else if new_state.position < -.max_position then "right"
        else direction
      in

      clear_screen ();                        (* Clear the screen for animation *)
      draw_cart_pole new_state;               (* Draw the cart and pole *)

      Unix.sleepf time_step;  (* Small delay to simulate real-time *)

      loop new_state (n - 1) new_direction
  in
  (* Initial state with the pole nearly upright *)
  let initial_state = { position = 0.0; velocity = 0.0; angle = 0.1; angular_velocity = 0.0 } in
  loop initial_state steps "right"   (* Start moving right *)

(* Run the simulation for 500 steps *)
let animate () = simulate_cart_pole 100
