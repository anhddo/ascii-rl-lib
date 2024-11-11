let rec loop (state : state_t) = 
  let action = QLearning.getNextStep state
  in 
  let next_state, reward, done_ = Simulation.nextStep state action  
  in
  Graphics.draw(next_state);
  loop next_state


