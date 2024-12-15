let () =
  let module Cartpole = Cartpole.Make (struct
    let render = true
  end) in
  Random.self_init ();
  let rec run_episodes count =
    if count > 0 then (
      let _, internal_state = Cartpole.reset () in
      Cartpole.simulate internal_state;
      run_episodes (count - 1)
    ) else
      ()
  in
  run_episodes 10
