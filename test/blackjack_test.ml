open Core
open OUnit2

module Config = struct
  let simulation_name = "blackjack"
  let render = false
end

module BlackjackSet = Blackjack.Make(Config)
module Helper_tests =
  struct

  let draw_card_test _= 
    let invariant (_ : int) =
      let rand = BlackjackSet.draw_card()
      in 
      assert_bool "result not within bounds" (Float.(<=) rand 10. && Float.(>=) rand 1.)
    in
    Quickcheck.test (Int.gen_incl 0 0) ~f:invariant;;

    let series = 
      "Helper Function Tests" >::: [ 
        "Draw Card Tests" >:: draw_card_test;
      ]
  end

(* Likely, these both will have to be Quickchecks *)
module Action_tests = 
  struct 


  let create_tests _ = 
    let invariant (_ : int) =
      assert_equal [ 0.0; 0.0; 0.0 ] @@ BlackjackSet.create()
    in
    Quickcheck.test (Int.gen_incl 0 0) ~f:invariant;; 
  let reset_tests _ = 
    let invariant (_ : int) =
      let (_, result) = BlackjackSet.reset()
      in
      match result with 
      | total_sum :: dealer_card :: has_ace :: [] ->
        assert_bool "total sum out of bounds" (Float.(>=) total_sum 2. && Float.(<=) total_sum 21.);
        assert_bool "dealer card out of bounds" (Float.(>=) dealer_card 1. && Float.(<=) dealer_card 11.);
        assert_bool "has_ace not a boolean" (Float.(=) has_ace 0. || Float.(=) has_ace 1.);
      | _ -> failwith "improper state"
    in
    Quickcheck.test (Int.gen_incl 0 0) ~f:invariant;; 
    
  let step_tests _ =
    let invariant (move_int : int) =
      let move = Float.of_int move_int
      in
      let (_, result) = BlackjackSet.reset()
      in
      let response = BlackjackSet.step result [move]
      in
      match (response.internal_state, response.reward, response.terminated) with 
      | (player_sum :: dealer_face_card :: has_ace :: [], reward, terminated) ->
        assert_bool "player can't win and bust" @@ not (Float.(>) player_sum 21. && Float.(=) reward 1.);
        assert_bool "player can't continue and bust" @@ not (Float.(>) player_sum 21. && Float.(=) reward 0.);
        assert_bool "total sum out of bounds" (Float.(>=) player_sum 2. && Float.(<=) player_sum 21. || terminated);
        assert_bool "dealer card out of bounds" (Float.(>=) dealer_face_card 1. && Float.(<=) dealer_face_card 11.);
        assert_bool "has_ace not a boolean" (Float.(=) has_ace 0. || Float.(=) has_ace 1.);
        assert_bool "reward not a set value" (Float.(=) reward 0. || Float.(=) reward 1. || Float.(=) reward (-1.));
        assert_bool "set reward should be truncated" @@ not ((Float.(=) reward (-1.) || Float.(=) reward (1.)) && not terminated)
      | (_ , _ , _) -> failwith "improper state"
    in
    Quickcheck.test (Int.gen_incl 0 1) ~f:invariant;; 

    let series = 
      "Action Tests" >::: [
        "Create Tests" >:: create_tests;
        "Reset Tests " >:: reset_tests;
        "Step Tests" >:: step_tests
      ]
  end

let series =
  "Blackjack Simutation Tests" >:::
  [ 
    Helper_tests.series;
    Action_tests.series
  ]

let () = run_test_tt_main series