let episode = ref 1
let algo = ref "qlearning"
 
 
let main =
begin
let speclist = [
("-episode", Arg.Int (fun n -> episode := n), "Sets maximum number of files to list");
("-algo", Arg.String (fun s -> algo := s), "Sets maximum number of files to list");
]
in Arg.parse speclist print_endline "";
print_int !episode;
  Qlearning.train (Qlearning.init_environment "CartPole-v1" true) !episode;
end
 
let () = main