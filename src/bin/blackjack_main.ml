let () =
  let module Blackjack = Blackjack.Make (struct
    let render = true
  end) in
  Blackjack.simulate ()
