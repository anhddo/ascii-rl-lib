open Core
open OUnit2

module Config = struct
  let simulation_name = "cartpole"
  let render = false
end

module CartpoleSet = Cartpole.Make (Config)

(* Likely, these both will have to be Quickchecks *)
module Action_tests = struct
  let create_tests _ =
    let invariant (_ : int) =
      assert_equal [ 0.0; 0.0; 0.0; 0.0; 0.0 ] @@ CartpoleSet.create ()
    in
    Quickcheck.test (Int.gen_incl 0 0) ~f:invariant

  let reset_tests _ =
    let invariant (_ : int) =
      let _, result = CartpoleSet.reset () in
      match result with
      | [ cart_location; cart_velocity; pole_angle; pole_velocity; steps; _ ] ->
          assert_bool "cart location improper start"
          @@ Float.( <= ) (Float.abs cart_location) 0.05;
          assert_bool "cart velocity improper start"
          @@ Float.( <= ) (Float.abs cart_velocity) 0.05;
          assert_bool "pole angle improper start"
          @@ Float.( <= ) (Float.abs pole_angle) 0.05;
          assert_bool "pole angular velocity improper start"
          @@ Float.( <= ) (Float.abs pole_velocity) 0.05;
          assert_bool "step not -1" @@ Float.( = ) (-1.) steps
      | _ -> failwith "improper state"
    in
    Quickcheck.test (Int.gen_incl 0 0) ~f:invariant

  let step_tests _ =
    let invariant (move_int : int) =
      let move = Float.of_int move_int in
      let _, result = CartpoleSet.reset () in
      let response = CartpoleSet.step result [ move ] in
      match (response.internal_state, response.reward, response.terminated) with
      | [ cart_location; _; pole_angle; _; steps; _ ], reward, terminated ->
          assert_bool "too extreme cart location"
          @@ Float.( <= ) cart_location 4.8;
          assert_bool "too extreme pole angle" @@ Float.( <= ) pole_angle 0.42;
          assert_bool "reward not a set value"
            (Float.( = ) reward 0. || Float.( = ) reward 1.);
          assert_bool "steps not a set value"
            (Float.( = ) steps 0. || Float.( = ) steps (-1.));
          assert_bool "set reward should be truncated"
          @@ not (Float.( = ) reward 0. && not terminated);
          assert_bool "terminated should have set step"
          @@ not (terminated && Float.( = ) steps 0.)
      | _, _, _ -> failwith "improper state"
    in
    Quickcheck.test (Int.gen_incl 0 1) ~f:invariant

  let series =
    "Action Tests"
    >::: [
           "Create Tests" >:: create_tests;
           "Reset Tests " >:: reset_tests;
           "Step Tests" >:: step_tests;
         ]
end

let series =
  "Cartpole Simutation Tests" >::: [ Action_tests.series ]

let () = run_test_tt_main series
