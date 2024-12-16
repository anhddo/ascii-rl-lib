let () =
  let module Cartpole1 = Cartpole.Make (struct
    let simulation_name = "cartpole"
    let render = true
  end) in
  Random.self_init ();
  let rec run_episodes count =
    if count > 0 then (
      let _, internal_state = Cartpole1.reset () in
      Cartpole1.simulate internal_state;
      run_episodes (count - 1)
    ) else
      ()
  in
  run_episodes 2

  