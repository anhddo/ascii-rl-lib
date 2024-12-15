let () =
  (* let module Config = struct let render = true end in *)
  let module Pendulum = Pendulum.Make (struct
    let render = true
  end) in
  Random.self_init ();
  let _, internal_state = Pendulum.reset () in
  Pendulum.simulate internal_state
