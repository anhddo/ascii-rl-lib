let episode = ref 1
let algo = ref "qlearning"
let env = ref "cartpole"
 
 
let main =
begin
let speclist = [
("-episode", Arg.Int (fun n -> episode := n), "number of episodes to train");
("-algo", Arg.String (fun s -> algo := s), "the algorithm to use");
("-env", Arg.String (fun s -> env := s), "the environment to use");
]
in Arg.parse speclist print_endline "";
print_int !episode;
  Qlearning.train !episode;
end
 
let () = main

(* This is just base code that will simuate the general idea of how our main loop will work 
    It will not compile or do anything useful.
    *)

(* let rec loop (state : state_t) = 
    let action = QLearning.getNextStep state
    in 
    let next_state, reward, done_ = Simulation.nextStep state action  
    in
    Graphics.draw(next_state);
    loop next_state *)