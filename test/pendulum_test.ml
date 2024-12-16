open Core
open OUnit2

module Config = struct
  let simulation_name = "pendulum"
  let render = false
end

module PendulumSet = Pendulum.Make(Config)

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
    Action_tests.series
  ]

let () = run_test_tt_main series