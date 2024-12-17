open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)

module Algo_config = struct
  let algo_name = "vpg"
  let model_path = "data/cartpole_vpg_test.ckpt"
  let episode = 2
  let learning_rate = 0.01
  let gamma = 0.99
end

module Cartpole_config = struct
  let simulation_name = "cartpole"
  let render = false
end

(* Command-line argument specification *)

(* Entry point *)

let tests =
  "Vpg tests"
  >::: [
         ( "discrete test" >:: fun _ ->
           let module Sim = Cartpole.Make (Cartpole_config) in
           let module Algo = Vpg.Make (Algo_config) (Sim) in
           assert_equal 1 1 );
         ( "continuous test" >:: fun _ ->
           let module Sim_env = struct
             let simulation_name = "pendulum"
             let render = false
           end in
           let module Sim = Pendulum.Make (Sim_env) in
           let module Algo = Vpg.Make (Algo_config) (Sim) in
           Algo.save_model ();
           let module Algo1 = Vpg.Make (Algo_config) (Sim) in
           Sys.remove Algo_config.model_path;
           assert_equal 1 1 );
         ( "train test" >:: fun _ ->
           let module Sim = Cartpole.Make (Cartpole_config) in
           let module Algo = Vpg.Make (Algo_config) (Sim) in
           Algo.train () );
       ]

let () = run_test_tt_main tests
