open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)


module Config = struct
  let render = false
end
module Cartpole_env = Cartpole.Make (Config)
module Pendulum_env = Pendulum.Make (Config)
module Cartpole_state_action = State_action.Make (Cartpole_env)

module Pendulum_state_action  = State_action.Make (Pendulum_env)



let test_func x y =
  (* Printf.printf  *)
  assert_equal x y ~msg:Printf.(sprintf "Expected %d, got %d" x y)

let tests =
  "Cartpole test"
  >::: [
         ( "bin test" >:: fun _ ->
           assert_equal 1 (Cartpole_state_action.value_to_bin 0.7 0. 1. 2) );
         ( "bin test1" >:: fun _ ->
           assert_equal 0 (Cartpole_state_action.value_to_bin 0.01 0. 1. 2) );
         ( "bin test2" >:: fun _ ->
           assert_equal 3 (Cartpole_state_action.value_to_bin 0.75 0. 1. 4) );
         ( "bin test3" >:: fun _ ->
           assert_equal 0 (Cartpole_state_action.value_to_bin (-0.75) 0. 1. 4) );
         ( "bin test4" >:: fun _ ->
           assert_equal 3 (Cartpole_state_action.value_to_bin 1.75 0. 1. 4) );
         ( "bin test5" >:: fun _ ->
           assert_equal 1 (Cartpole_state_action.value_to_bin (-3.59) (-4.) 4. 20) );
         ( "bin test6" >:: fun _ ->
           assert_equal 1 (Cartpole_state_action.value_to_bin 0.25 0. 1. 4) );
         ( "convert" >:: fun _ ->
           test_func 1
             (Cartpole_state_action.convert_state_to_bin
                [ -4.8; -4.0; -0.418; -3.59 ]) );
         ( "convert1" >:: fun _ ->
           test_func 401
             (Cartpole_state_action.convert_state_to_bin
                [ -4.8; -3.59; -0.418; -3.59 ]) );
         ( "bin test pendulum" >:: fun _ ->
           test_func 1 (Pendulum_state_action.value_to_bin (-7.19) (-8.) 8. 20) );
         ( "bin test pendulum1 " >:: fun _ ->
           test_func 3 (Pendulum_state_action.value_to_bin 1. (-1.) 1. 4) );
         ( "bin test pendulum2 " >:: fun _ ->
           test_func 0 (Pendulum_state_action.value_to_bin (-1.) (-1.) 1. 4) );
         ( "pendulum convert" >:: fun _ ->
           test_func 1
             (Pendulum_state_action.convert_state_to_bin [ -1.; -1.; -7.19 ]) );
       ]

let () = run_test_tt_main tests
