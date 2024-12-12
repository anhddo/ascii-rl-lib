(* blackjack_main.ml *)

let () =
  let module Blackjack = Blackjack.Make (struct
    let simulation_name = "blackjack"
    let render = true
  end) in
  Blackjack.simulate ()
