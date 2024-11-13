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
  Qlearning.train (Qlearning.init_environment "CartPole-v1" true) !episode;
end
 
let () = main