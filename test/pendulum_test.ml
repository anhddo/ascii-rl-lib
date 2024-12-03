open Core
open OUnit2

module Config = struct
  let render = false
end

module PendulumSet = Pendulum.Make(Config)
module Helper_tests =
  struct

    let square_tests _ =
      assert_equal 0. @@ PendulumSet.square 0.;
      assert_equal 9. @@ PendulumSet.square 3.;
      assert_equal 4. @@ PendulumSet.square (-2.);
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.21 -. PendulumSet.square 1.1)) 0.0001; 
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.69 -. PendulumSet.square (-1.3))) 0.0001

    let modulo_tests _ = 
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1.1 -. PendulumSet.modulo 3.3 2.2)) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (1. -. PendulumSet.modulo (-3.5) 1.5)) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs ((-0.1) -. PendulumSet.modulo (-1.3) (-1.2))) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs ((-0.1) -. PendulumSet.modulo (1.9) (-0.5))) 0.0001;
      assert_bool "Failed Percision"  @@  Float.(<=) (Float.abs (0. -. PendulumSet.modulo (-3.) (-1.5))) 0.0001

    let normalize_angle_tests _ = 
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs (0. -. PendulumSet.normalize_angle (0.))) 0.0001;
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs ((Float.pi *. -3. /. 4.) -. PendulumSet.normalize_angle (Float.pi *. 5. /. 4.))) 0.0001;
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs ((Float.pi *. -1. /. 4.) -. PendulumSet.normalize_angle (Float.pi *. 7. /. 4.))) 0.0001;
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs ((Float.pi /. 4.) -. PendulumSet.normalize_angle (Float.pi *. 9. /. 4.))) 0.0001;
      assert_bool "Failed Percision" @@ Float.(<=) (Float.abs ((Float.pi *. 3. /. 4.) -. PendulumSet.normalize_angle (Float.pi *. -5. /. 4.))) 0.0001

    let random_between_tests _= 
      let min_bound = -200.
      in
      let max_bound = 300.
      in
      let invariant (_ : int) =
        let rand = PendulumSet.random_between min_bound max_bound
        in 
        assert_bool "result not within bounds" (Float.(<=) rand max_bound && Float.(>=) rand min_bound)
      in
      Quickcheck.test (Int.gen_incl 0 0) ~f:invariant;;

    let clip_tests _ = 
      assert_equal 5 @@ PendulumSet.clip 0 10 5;
      assert_equal 10 @@ PendulumSet.clip 0 10 100;
      assert_equal (-1) @@ PendulumSet.clip (-1) 305 (-305);
      assert_equal "c" @@ PendulumSet.clip "c" "z" "a";
      assert_equal "e" @@ PendulumSet.clip "c" "z" "e";
      assert_equal "f" @@ PendulumSet.clip "c" "f" "r"

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

  let create_tests _ = 
    let invariant (_ : int) =
      assert_equal [ 0.0; 0.0; 0.0 ] @@ PendulumSet.create()
    in
    Quickcheck.test (Int.gen_incl 0 0) ~f:invariant;; 

  let reset_tests _ = 
    let invariant (_ : int) =
      let (feature_result, result) = PendulumSet.reset()
      in
      match (feature_result, result) with 
      | (x_point :: y_point :: ang_speed_a :: [], angle :: ang_speed_b :: timestep :: []) ->
        assert_bool "x_point improper start" @@ Float.(<=) (Float.abs x_point) 1.;
        assert_bool "y_point improper start" @@ Float.(<=) (Float.abs y_point) 1.;
        assert_bool "x_point improper start" @@ Float.(>=) (Float.abs x_point) (-1.);
        assert_bool "y_point improper start" @@ Float.(>=) (Float.abs y_point) (-1.);
        assert_bool "ang_speed unequal" @@ Float.(=) ang_speed_a ang_speed_b;
        assert_bool "ang_speed improper start" @@ Float.(<=) (Float.abs ang_speed_a) 1.;
        assert_bool "ang_speed improper start" @@ Float.(>=) (Float.abs ang_speed_a) (-1.);
        assert_bool "angle improper start" @@ Float.(<=) (Float.abs angle) Float.pi;
        assert_bool "angle improper start" @@ Float.(>=) (Float.abs angle) (Float.neg Float.pi);
        assert_bool "timestep not 0" @@ Float.(=) 0. timestep;
      | (_ , _ ) -> failwith "improper state"
    in
    Quickcheck.test (Int.gen_incl 0 0) ~f:invariant;; 
  
  let step_tests _ =
    let invariant (applied_torque : float) =
      let (_, result) = PendulumSet.reset()
      in
      let response = PendulumSet.step result [applied_torque]
      in
      match (response.observation, response.internal_state, response.reward) with 
      | (x_point :: y_point :: ang_speed_a :: [], angle :: ang_speed_b :: timestep :: [], reward) ->
        assert_bool "x_point too great" @@ Float.(<=) (Float.abs x_point) 1.;
        assert_bool "y_point too great " @@ Float.(<=) (Float.abs y_point) 1.;
        assert_bool "x_point too small" @@ Float.(>=) (Float.abs x_point) (-1.);
        assert_bool "y_point too small" @@ Float.(>=) (Float.abs y_point) (-1.);
        assert_bool "ang_speed unequal" @@ Float.(=) ang_speed_a ang_speed_b;
        assert_bool "ang_speed too large" @@ Float.(<=) (Float.abs ang_speed_a) 8.;
        assert_bool "ang_speed too small" @@ Float.(>=) (Float.abs ang_speed_a) (-8.);
        assert_bool "angle too large" @@ Float.(<=) (Float.abs angle) Float.pi;
        assert_bool "angle too small" @@ Float.(>=) (Float.abs angle) (Float.neg Float.pi);
        assert_bool "timestep not 1" @@ Float.(=) 1. timestep;
        assert_bool "reward too great " @@ Float.(<=) reward 0.
      | (_ , _ , _) -> failwith "improper state"
    in
    Quickcheck.test (Float.gen_incl (-2.) 2.) ~f:invariant;; 

    let series = 
      "Action Tests" >::: [
        "Create Tests " >:: create_tests;
        "Reset Tests " >:: reset_tests;
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