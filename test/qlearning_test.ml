open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)

open Qlearning

let tests =
  "test suite"
  >::: [
         ("bin test" >:: fun _ -> assert_equal 1 (value_to_bin 0.7 0. 1. 2));
         ("bin test1" >:: fun _ -> assert_equal 0 (value_to_bin 0.01 0. 1. 2));
         ("bin test2" >:: fun _ -> assert_equal 3 (value_to_bin 0.75 0. 1. 4));
         ("bin test3" >:: fun _ -> assert_equal 0 (value_to_bin (-0.75) 0. 1. 4));
         ("bin test4" >:: fun _ -> assert_equal 3 (value_to_bin 1.75 0. 1. 4));
         ("bin test5" >:: fun _ -> assert_equal 1 (value_to_bin (-3.59) (-4.) 4. 20));
         ("bin test6" >:: fun _ -> assert_equal 1 (value_to_bin 0.25 0. 1. 4));
         ( "convert" >:: fun _ ->
           assert_equal 1
             (convert_state_to_bin [ -4.8; -4.0; -0.418; -3.59 ])
         );
         ( "convert1" >:: fun _ ->
           assert_equal 401
             (convert_state_to_bin [ -4.8; -3.59; -0.418; -3.59 ])
         );
       ]

let () = run_test_tt_main tests
