[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

type t = float list (* Length is 2 | [location, ang_speed ] *)

type action =
  float list (* Length is 1 | [amount of torque to apply between -2 and 2] *)

(* observation : the new state of the simulation; the next step call should use this value *)
(* reward : maximum reward is 0, achieved when pendulum is perfectly balanced *)
(* terminated : idk, error handling? *)
(* truncated : idk *)
(* info : error handling, nothing for now *)
type response = {
  observation : t;
  reward : float;
  terminated : bool;
  truncated : bool;
  info : string;
}

let () = Py.initialize ()
let gym = Py.import "gymnasium"

let create () : t = [ 0.; 0. ]

let reset (action : int) : t * string =
  let random_starting_angle = Random.float (2. *. Float.pi) -. Float.pi in
  let random_starting_angular_speed = Random.float 2. -. 1. in
  ([ random_starting_angle; random_starting_angular_speed ], "Simulation Begun")
(* let reset env =
   let reset_fn' = Py.Object.get_attr_string env "reset" in
   let result = Py.Callable.to_function (Core.Option.value_exn reset_fn') [||] in
   let result, _ = Py.Tuple.to_tuple2 result in
   (*convert to list of float*)
   let state = Py.List.to_list_map Py.Float.to_float result in
   state *)

(* let stepn env action =
   let step_fn' = Py.Object.get_attr_string env "step" in
   let result =
     Py.Callable.to_function
       (Core.Option.value_exn step_fn')
       [| Py.Int.of_int @@ action |]
   in
   let _state, _reward, _is_done, _truncated, _ = Py.Tuple.to_tuple5 result in
   ( Py.List.to_list_map Py.Float.to_float _state,
     Py.Float.to_float _reward,
     Py.Bool.to_bool _is_done,
     _truncated,
     None ) *)

let reset (sim : t) : t * string =
  let random_starting_angle = Random.float (2. *. Float.pi) -. Float.pi in
  let random_starting_angular_speed = Random.float 2. -. 1. in
  ([ random_starting_angle; random_starting_angular_speed ], "Simulation Begun")
(* {
     observation = [ random_starting_angle; random_starting_angular_speed ];
     reward = 0.;
     terminated = false;
     truncated = false;
     info = "Simulation Begun";
   } *)

let step (act : action) : response = failwith "todo"
let render (sim : t) : char list = failwith "todo"

(*
Theoretically, after we finish implementing one of these, the reinforcement learning models can start training
- pendulum and likely all others have full documentation on how these work

We can then get to work on making a UI to these, or making more

*)

let init_environment (str: string) =
  Py.Module.get_function gym "make" [| Py.String.of_string str |]

let reset_fn env =
  let reset_fn' = Py.Object.get_attr_string env "reset" in
  let result = Py.Callable.to_function (Core.Option.value_exn reset_fn') [||] in
  let result, _ = Py.Tuple.to_tuple2 result in
  (*convert to list of float*)
  let state = Py.List.to_list_map Py.Float.to_float result in
  state

let step_fn env action =
  let step_fn' = Py.Object.get_attr_string env "step" in
  let result =
    Py.Callable.to_function
      (Core.Option.value_exn step_fn')
      [| Py.Int.of_int @@ action |]
  in
  let _state, _reward, _is_done, _truncated, _ = Py.Tuple.to_tuple5 result in
  ( Py.List.to_list_map Py.Float.to_float _state,
    Py.Float.to_float _reward,
    Py.Bool.to_bool _is_done,
    _truncated,
    None )

(* implement tabular q learning *)
let value_to_bin (value : float) (low : float) (high : float) (num_bins : int) :
    int =
  if value < low then 0
  else if value > high then num_bins - 1
  else
    let bin_width = (high -. low) /. float_of_int num_bins in
    let bin = (value -. low) /. bin_width in
    int_of_float bin

type bin = { low : float; high : float; num_bins : int }

let state_to_bin_config : bin list =
  [
    { low = -4.8; high = 4.8; num_bins = 20 };
    { low = -4.0; high = 4.0; num_bins = 20 };
    { low = -0.418; high = 0.418; num_bins = 20 };
    { low = -4.0; high = 4.0; num_bins = 20 };
  ]

let rec four_float_to_bin (state : float list) (bin_config : bin list) :
    int list =
  match (state, bin_config) with
  | sh :: st, bh :: bt ->
      value_to_bin sh bh.low bh.high bh.num_bins :: four_float_to_bin st bt
  | [], [] -> []
  | _ -> failwith "State and bin configuration lengths do not match"

let convert_state_to_bin (state : float list) : int =
  let state_bin = four_float_to_bin state state_to_bin_config in
  let rec convert_state_to_bin' (state : int list) (n : int) : int =
    match state with
    | [] -> 0
    | h :: t ->
        int_of_float (float_of_int h *. Float.pow 20.0 (float_of_int n))
        + convert_state_to_bin' t (n - 1)
  in
  convert_state_to_bin' state_bin 3

let q_table = Array.make_matrix (int_of_float @@ Float.pow 20. 4.) 2 0.0

let train env (episode : int) =
  (* let env = env_render in *)
  let learning_rate = 0.1 in

  (* let state = reset_fn env in *)
  (* let dimx = int_of_float @@ Float.pow 20. 4. in *)
  let rec loop' (episode : int) (state : float list) (reward_ep : float) =
    let action =
      if Float.compare (Random.float 1.0) 0.1 = -1 then Random.int 2
      else
        let state_bin = convert_state_to_bin state in
        if q_table.(state_bin).(0) > q_table.(state_bin).(1) then 0 else 1
    in
    let next_state, reward, is_done, _, _ = step_fn env action in
    let next_state_bin = convert_state_to_bin next_state in
    let state_bin = convert_state_to_bin state in
    (* update q table*)
    let _ =
      if is_done then q_table.(state_bin).(action) <- reward
      else
        let max_q =
          max q_table.(next_state_bin).(0) q_table.(next_state_bin).(1)
        in
        q_table.(state_bin).(action) <-
          ((1. -. learning_rate) *. (reward +. (0.99 *. max_q)))
          +. (learning_rate *. q_table.(state_bin).(action))
    in
    let state = next_state in
    (* Printf.printf "%f \n " reward_ep; *)
    let reward_ep = reward_ep +. reward in
    (* Printf.printf "%b %f %f \n " is_done reward reward_ep; *)
    if is_done then (
      let state = reset_fn env in
      (* report total reward *)
      if episode mod 100 = 0 then
        Printf.printf "total reward:%d %f \n" episode reward_ep;
      loop' (episode - 1) state 0.0)
    else if episode > 0 then loop' episode state reward_ep
    else ()
    (* else q_table *)
  in
  loop' episode (reset_fn env) 0.0
