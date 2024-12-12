open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)


module Config = struct
  let simulation_name = ""
  let render = false
end
module Cartpole_env = Cartpole.Make (Config)
module Pendulum_env = Pendulum.Make (Config)
module Cartpole_state_action = State_action.Make (Cartpole_env)

module Pendulum_state_action  = State_action.Make (Pendulum_env)


(* next_state: (1. -1.45140206468e-008 -0.120906035639)
next_state_bin: 8189
Fatal error: exception Invalid_argument("index out of bounds") *)
let test_func x y =
  (* Printf.printf  *)
  assert_equal x y ~msg:Printf.(sprintf "Expected %d, got %d" x y)

let tests =
  "State_action tests"
  >::: [
         ( "bin test" >:: fun _ ->
           assert_equal 1 (Cartpole_state_action.value_to_bin 0.7 0. 1. 2) );
         ( "bin test1.1" >:: fun _ ->
           test_func 0 (Cartpole_state_action.value_to_bin 0.0 0. 1. 2) );
         ( "bin test1.2" >:: fun _ ->
           test_func 1 (Cartpole_state_action.value_to_bin 1.0 0. 1. 2) );
         ( "bin test1" >:: fun _ ->
           test_func 0 (Cartpole_state_action.value_to_bin 0.01 0. 1. 2) );
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
         ( "bin test pendulum" >:: fun _ ->
           test_func 19 (Pendulum_state_action.value_to_bin (8.3) (-8.) 8. 20) );
         ( "bin test pendulum" >:: fun _ ->
           test_func 0 (Pendulum_state_action.value_to_bin (-8.3) (-8.) 8. 20) );
         ( "bin test pendulum1 " >:: fun _ ->
           test_func 3 (Pendulum_state_action.value_to_bin 1. (-1.) 1. 4) );
         ( "bin test pendulum2 " >:: fun _ ->
           test_func 0 (Pendulum_state_action.value_to_bin (-1.) (-1.) 1. 4) );
         ( "bin test pendulum3 " >:: fun _ ->
           test_func 19 (Pendulum_state_action.value_to_bin 1. (-1.) 1. 20) );
         ( "pendulum convert" >:: fun _ ->
           test_func 1
             (Pendulum_state_action.convert_state_to_bin [ -1.; -1.; -7.19 ]) );
         ( "pendulum convert1" >:: fun _ ->
          let v = Pendulum_state_action.convert_state_to_bin [1.; -1.25208246222e-008; 0.314987382668] in
           assert_bool "test" 
             (Int.compare 8000  v = 1) );
         ( "pendulum convert2" >:: fun _ ->
          let v = Pendulum_state_action.convert_state_to_bin [0.999993687422; -0.00355318389298; 0.152762888634] in
           assert_equal 7790 v);
         ( "pendulum convert3" >:: fun _ ->
          let v = Pendulum_state_action.convert_state_to_bin [0.999896233083; -0.01440566092; -0.0835999333127] in
          print_int v;
           assert_equal 7789 v)
       ]

let () = run_test_tt_main tests
