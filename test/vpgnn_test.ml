open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)

module Algo_config = struct
  let algo_name = "vpgnn"
  let model_path = "data/cartpole_vpgnn_test.ckpt"
  let episode = 2
  let learning_rate = 0.05
  let gamma = 0.9
end

module Cartpole_config = struct
  let simulation_name = "cartpole"
  let render = false
end

(* Command-line argument specification *)

(* Entry point *)

let tests =
  "Vpgnn tests"
  >::: [
         ( "discrete test" >:: fun _ ->
           let module Sim = Cartpole.Make (Cartpole_config) in
           let module Algo = Vpgnn.Make (Algo_config) (Sim) in
           assert_equal 1 1 );
         ( "continuous test" >:: fun _ ->
           let module Sim = Cartpole.Make (Cartpole_config) in
           let module Algo = Vpgnn.Make (Algo_config) (Sim) in
           Algo.save_model ();
           let module Algo1 = Vpgnn.Make (Algo_config) (Sim) in
           Sys.remove Algo_config.model_path;
           assert_equal 1 1 );
         ( "train test" >:: fun _ ->
           let module Sim = Cartpole.Make (Cartpole_config) in
           let module Algo = Vpgnn.Make (Algo_config) (Sim) in
           Algo.train () );
       ]

let () = run_test_tt_main tests
