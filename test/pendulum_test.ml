open Core
open OUnit2


module Helper_tests =
  struct

    let square_tests _ =
      assert_equal 0. @@ Pendulum.square 0.;
      assert_equal 9. @@ Pendulum.square 3.;
      assert_equal 4. @@ Pendulum.square (-2.);
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.21 -. Pendulum.square 1.1)) 0.0001; 
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.69 -. Pendulum.square (-1.3))) 0.0001

    let modulo_tests _ = 
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.1 -. Pendulum.modulo 3.3 2.2)) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1. -. Pendulum.modulo (-3.5) 1.5)) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs ((-0.1) -. Pendulum.modulo (-1.3) (-1.2))) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs ((-0.1) -. Pendulum.modulo (1.9) (-0.5))) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (0. -. Pendulum.modulo (-3.) (-1.5))) 0.0001

    let normalize_angle_tests _ = 
      assert_equal 0 0 

    let random_between_tests _ = 
      assert_equal 0 0 

    let clip_tests _ = 
      assert_equal 0 0 

    let series = 
      "Helper Function Tests" >::: [ 
        "Square Tests" >:: square_tests;
        "Modulo Tests" >:: modulo_tests;
        "Normalize Angle Tests" >:: normalize_angle_tests;
        "Random Between Tests" >:: random_between_tests;
        "Clip Tests" >:: clip_tests
      ]
  end

(* Likely, these both will have to be Quickchecks *)
module Action_tests = 
  struct 

    let reset_tests _ = 
      assert_equal 0 0 
    
    let step_tests _ =
      assert_equal 0 0 

    let series = 
      "Action Tests" >::: [
        "Resetting Tests " >:: reset_tests;
        "Step Tests" >:: step_tests
      ]
  end

let series =
  "Pendulum Simutation Tests" >:::
  [ 
    Helper_tests.series;
    Action_tests.series
  ]

let () = run_test_tt_main series