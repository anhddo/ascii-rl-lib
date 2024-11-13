(*some sketch of our main loop*)
(* let rec loop (state) = 
  let action = QLearning.get_next_state state
  in 
  let next_state, reward, done_ = Simulation.step state action  
  in
  (* Graphics.draw(next_state); we will implement this graphics later *)
  loop next_state
 *)

