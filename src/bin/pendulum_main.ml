(* pendulum_main.ml *)

open Pendulum

let () =
  Random.self_init ();
  let init_state = reset () in
  simulate init_state
