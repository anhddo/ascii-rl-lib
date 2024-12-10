open Core
open OUnit2

module Config = struct
  let render = false
end

module CartpoleSet = Cartpole.Make(Config)
module Helper_tests =
  struct

  let dummy_tests _ = 
    assert_equal 0 0


    let series = 
      "Helper Function Tests" >::: [ 
        "Dummy Tests" >:: dummy_tests;
      ]
  end

(* Likely, these both will have to be Quickchecks *)
module Action_tests = 
  struct 

  let create_tests _ = 
    assert_equal 0 0; ignore()

  let reset_tests _ = 
    assert_equal 0 0 
    
  let step_tests _ =
    assert_equal 0 0 

    let series = 
      "Action Tests" >::: [
        "Create Tests" >:: create_tests;
        "Reset Tests " >:: reset_tests;
        "Step Tests" >:: step_tests
      ]
  end

let series =
  "Cartpole Simutation Tests" >:::
  [ 
    Helper_tests.series;
    Action_tests.series
  ]

let () = run_test_tt_main series