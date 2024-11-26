open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)


module Config = struct
  include Simulation.Config

  let name = "CartPole-v1"
  let render = false
end

module Cartpole_config = Qlearning.Make_config (Config)

module Config1 = struct
  include Simulation.Config

  let name = "Pendulum-v1"
  let render = false
end

module Pendulum_config = Qlearning.Make_config (Config1)

let test_func x y =
  (* Printf.printf  *)
  assert_equal x y ~msg:Printf.(sprintf "Expected %d, got %d" x y)

let tests =
  "Cartpole test"
  >::: [
         ( "bin test" >:: fun _ ->
           assert_equal 1 (Cartpole_config.value_to_bin 0.7 0. 1. 2) );
         ( "bin test1" >:: fun _ ->
           assert_equal 0 (Cartpole_config.value_to_bin 0.01 0. 1. 2) );
         ( "bin test2" >:: fun _ ->
           assert_equal 3 (Cartpole_config.value_to_bin 0.75 0. 1. 4) );
         ( "bin test3" >:: fun _ ->
           assert_equal 0 (Cartpole_config.value_to_bin (-0.75) 0. 1. 4) );
         ( "bin test4" >:: fun _ ->
           assert_equal 3 (Cartpole_config.value_to_bin 1.75 0. 1. 4) );
         ( "bin test5" >:: fun _ ->
           assert_equal 1 (Cartpole_config.value_to_bin (-3.59) (-4.) 4. 20) );
         ( "bin test6" >:: fun _ ->
           assert_equal 1 (Cartpole_config.value_to_bin 0.25 0. 1. 4) );
         ( "convert" >:: fun _ ->
           test_func 1
             (Cartpole_config.convert_state_to_bin
                [ -4.8; -4.0; -0.418; -3.59 ]) );
         ( "convert1" >:: fun _ ->
           test_func 401
             (Cartpole_config.convert_state_to_bin
                [ -4.8; -3.59; -0.418; -3.59 ]) );
         ( "bin test pendulum" >:: fun _ ->
           test_func 1 (Pendulum_config.value_to_bin (-7.19) (-8.) 8. 20) );
         ( "bin test pendulum1 " >:: fun _ ->
           test_func 3 (Pendulum_config.value_to_bin 1. (-1.) 1. 4) );
         ( "bin test pendulum2 " >:: fun _ ->
           test_func 0 (Pendulum_config.value_to_bin (-1.) (-1.) 1. 4) );
         ( "pendulum convert" >:: fun _ ->
           test_func 1
             (Pendulum_config.convert_state_to_bin [ -1.; -1.; -7.19 ]) );
       ]

let () = run_test_tt_main tests
