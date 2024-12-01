open Core
open OUnit2

module Config = struct
  let render = false
end

module BlackjackSet = Blackjack.Make(Config)
module Helper_tests =
  struct

  let clip_tests _ = 
    assert_equal 5 @@ BlackjackSet.clip 0 10 5;
    assert_equal 10 @@ BlackjackSet.clip 0 10 100;
    assert_equal (-1) @@ BlackjackSet.clip (-1) 305 (-305);
    assert_equal "c" @@ BlackjackSet.clip "c" "z" "a";
    assert_equal "e" @@ BlackjackSet.clip "c" "z" "e";
    assert_equal "f" @@ BlackjackSet.clip "c" "f" "r"

    let series = 
      "Helper Function Tests" >::: [ 
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