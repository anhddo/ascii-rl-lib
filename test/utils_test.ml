open OUnit2
(* we usually open OUnit2 since it is pervasively used in test files *)

open Utils
open Core

module Helper_tests =
  struct

    let square_tests _ =
      assert_equal 0. @@ square 0.;
      assert_equal 9. @@ square 3.;
      assert_equal 4. @@ square (-2.);
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.21 -. square 1.1)) 0.0001; 
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.69 -. square (-1.3))) 0.0001

    let modulo_tests _ = 
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.1 -. modulo 3.3 2.2)) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1. -. modulo (-3.5) 1.5)) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs ((-0.1) -. modulo (-1.3) (-1.2))) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs ((-0.1) -. modulo (1.9) (-0.5))) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (0. -. modulo (-3.) (-1.5))) 0.0001

    let normalize_angle_tests _ = 
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs (0. -. normalize_angle (0.))) 0.0001;
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs ((Float.pi *. -3. /. 4.) -. normalize_angle (Float.pi *. 5. /. 4.))) 0.0001;
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs ((Float.pi *. -1. /. 4.) -. normalize_angle (Float.pi *. 7. /. 4.))) 0.0001;
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs ((Float.pi /. 4.) -. normalize_angle (Float.pi *. 9. /. 4.))) 0.0001;
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs ((Float.pi *. 3. /. 4.) -. normalize_angle (Float.pi *. -5. /. 4.))) 0.0001

    let random_between_tests _= 
      let min_bound = -200.
      in
      let max_bound = 300.
      in
      let invariant (_ : int) =
        let rand = random_between min_bound max_bound
        in 
        assert_bool "result not within bounds" (Float.(<=) rand max_bound && Float.(>=) rand min_bound)
      in
      Quickcheck.test (Int.gen_incl 0 0) ~f:invariant;;

    let clip_tests _ = 
      assert_equal 5 @@ clip 0 10 5;
      assert_equal 10 @@ clip 0 10 100;
      assert_equal (-1) @@ clip (-1) 305 (-305);
      assert_equal "c" @@ clip "c" "z" "a";
      assert_equal "e" @@ clip "c" "z" "e";
      assert_equal "f" @@ clip "c" "f" "r"

    let series = 
      "Helper Function Tests" >::: [ 
        "Square Tests" >:: square_tests;
        "Modulo Tests" >:: modulo_tests;
        "Normalize Angle Tests" >:: normalize_angle_tests;
        "Random Between Tests" >:: random_between_tests;
        "Clip Tests" >:: clip_tests
      ]
  end

let tests =
  "State_action tests"
  >::: [
         ( "float argmax test1" >:: fun _ -> assert_equal 2 (float_argmax [ 1.0; 2.0; 3.0 ]) );
         ( "float argmax test2" >:: fun _ -> assert_equal 0 (float_argmax [ 3.0; 2.0; 1.0 ]) );
         Helper_tests.series
       ]

let () = run_test_tt_main tests
