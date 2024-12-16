open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)

open Utils

let tests =
  "State_action tests"
  >::: [
         ( "float argmax test1" >:: fun _ -> assert_equal 2 (float_argmax [ 1.0; 2.0; 3.0 ]) );
         ( "float argmax test2" >:: fun _ -> assert_equal 0 (float_argmax [ 3.0; 2.0; 1.0 ]) );
       ]

let () = run_test_tt_main tests
