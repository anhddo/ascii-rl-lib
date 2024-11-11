(* Physics parameters for the Lunar Lander simulation *)
let gravity = -1.62          (* Lunar gravity (m/s^2) *)
let max_thrust = 2.5         (* Maximum thrust force (m/s^2) *)
let time_step = 0.01          (* Time step for simulation (s) *)

(* Lander state *)
type lander = {
  mutable x : float;        (* Horizontal position *)
  mutable y : float;        (* Vertical position *)
  mutable vx : float;       (* Horizontal velocity *)
  mutable vy : float;       (* Vertical velocity *)
  mutable angle : float;    (* Rotation angle (radians) *)
}

(* Initialize the Lunar Lander state *)
let initialize_lander () = {
  x = 0.0;
  y = 50.0;       (* Start the lander at height 50 meters *)
  vx = 0.0;
  vy = 0.0;
  angle = 0.0;   (* Facing upwards *)
}

(* Autopilot control logic *)
let autopilot lander =
  let desired_angle = atan2 (-.lander.vx) gravity in
  let angle_change = desired_angle -. lander.angle in
  let thrust = 
    if lander.vy < -2.0 then max_thrust *. 0.7       (* Apply more thrust if falling too quickly *)
    else if lander.y > 5.0 then max_thrust *. 0.3    (* Moderate thrust to control descent *)
    else 0.0                                         (* No thrust close to the ground *)
  in
  (thrust, angle_change)

(* Update lander's state based on thrust and rotation *)
let update_lander lander thrust angle_change =
  (* Update angle with a limited range *)
  lander.angle <- lander.angle +. angle_change;
  if lander.angle > 3.14 then lander.angle <- -3.14;
  if lander.angle < -3.14 then lander.angle <- 3.14;

  (* Calculate thrust components *)
  let thrust_x = thrust *. cos lander.angle in
  let thrust_y = thrust *. sin lander.angle in

  (* Update velocities with gravity and thrust *)
  lander.vx <- lander.vx +. (thrust_x *. time_step);
  lander.vy <- lander.vy +. (thrust_y *. time_step) +. (gravity *. time_step);

  (* Update positions *)
  lander.x <- lander.x +. (lander.vx *. time_step);
  lander.y <- lander.y +. (lander.vy *. time_step);

  (* Boundary conditions for ground landing *)
  if lander.y <= 0.0 then begin
    lander.y <- 0.0;
    lander.vy <- 0.0;
    lander.vx <- 0.0;
  end

(* Function to visualize the lander's position in the terminal *)
let visualize_lander lander thrust_active =
  let width = 40 in
  let height = 20 in
  let screen = Array.make_matrix height width ' ' in

  (* Draw ground *)
  for i = 0 to width - 1 do
    screen.(height - 1).(i) <- '_'
  done;

  (* Calculate position on screen *)
  let x_screen = int_of_float (lander.x *. 0.2 +. float width /. 2.0) in
  let y_screen = height - 2 - int_of_float (lander.y *. 0.2) in

  (* Draw rocket shape with fins and particles *)
  if y_screen >= 2 && y_screen < height && x_screen >= 1 && x_screen < width - 1 then begin
    (* Rocket body *)
    screen.(y_screen).(x_screen) <- '|';
    screen.(y_screen - 1).(x_screen) <- '|';
    screen.(y_screen - 2).(x_screen - 1) <- '>';
    screen.(y_screen - 2).(x_screen + 1) <- '<';

    (* Thrust particles *)
    if thrust_active then begin
      screen.(y_screen + 1).(x_screen) <- '*';
      if x_screen > 1 then screen.(y_screen + 2).(x_screen - 1) <- '*';
      if x_screen < width - 2 then screen.(y_screen + 2).(x_screen + 1) <- '*';
    end
  end;

  (* Render screen *)
  print_string "\x1b[2J"; (* Clear screen *)
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      print_char screen.(y).(x)
    done;
    print_newline ()
  done;
  flush stdout

(* Main simulation loop *)
let simulate_lander () =
  let lander = initialize_lander () in
  let rec loop () =
    (* Autopilot controls *)
    let (thrust, angle_change) = autopilot lander in
    let thrust_active = thrust > 0.0 in

    (* Update lander state *)
    update_lander lander thrust angle_change;

    (* Visualize lander with rocket shape and particles *)
    visualize_lander lander thrust_active;
    Printf.printf "Lander state: x=%.2f, y=%.2f, vx=%.2f, vy=%.2f, angle=%.2f\n"
      lander.x lander.y lander.vx lander.vy lander.angle;

    (* Check for landing *)
    if lander.y <= 0.0 then
      if lander.vy > -2.0 && lander.vx < 1.0 then
        Printf.printf "Successful landing!\n"
      else
        Printf.printf "Crash landing.\n"
    else
      let () = Unix.sleepf time_step in  (* Small delay for animation *)
      loop ()
  in
  loop ()

(* Run the simulation *)
let animate() = simulate_lander ()
