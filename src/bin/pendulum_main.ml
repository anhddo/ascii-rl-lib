(* pendulum_main.ml *)

open Pendulum

let () =
  Random.self_init ();
  let _, internal_state = reset () in
  simulate internal_state
