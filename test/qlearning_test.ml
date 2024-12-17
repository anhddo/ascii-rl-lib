open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)

module Algo_config = struct
  let algo_name = "qlearning"
  let model_path = "data/cartpole_test.sexp"
  let episode = 200
  let learning_rate = 0.1
  let gamma = 0.9
end

module Cartpole_config = struct
  let simulation_name = "cartpole"
  let render = false
end

(* Command-line argument specification *)

(* Entry point *)

let tests =
  "State_action tests"
  >::: [
         ( "discrete test" >:: fun _ ->
           let module Sim = Cartpole.Make (Cartpole_config) in
           let module Algo = Qlearning.Make (Algo_config) (Sim) in
           assert_equal 1 1 );
         ( "continuous test" >:: fun _ ->
           let module Sim_env = struct
             let simulation_name = "pendulum"
             let render = false
           end in
           let module Sim = Pendulum.Make (Sim_env) in
           let module Algo = Qlearning.Make (Algo_config) (Sim) in
           Algo.save_model ();
           let module Algo1 = Qlearning.Make (Algo_config) (Sim) in
           Sys.remove Algo_config.model_path;
           assert_equal 1 1 );
         ( " train test" >:: fun _ ->
           let module Sim = Cartpole.Make (Cartpole_config) in
           let module Algo = Qlearning.Make (Algo_config) (Sim) in
           Algo.train () );
       ]

let () = run_test_tt_main tests
