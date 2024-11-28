open Core
open OUnit2

module Config = struct
  let render = false
end

module BlackjackSet = Blackjack.Make(Config)
module Helper_tests =
  struct

    let dummy_tests _ = 
      assert_equal Int.max_value Int.max_value

    let series = 
      "Helper Function Tests" >::: [ 
        "Dummy Tests" >:: dummy_tests
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