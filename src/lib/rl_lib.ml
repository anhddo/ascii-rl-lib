(* open Core *)

(* if not (Py.is_initialized ()) then Py.initialize () else (); *)
let () = Py.initialize()
let gym = Py.import "gymnasium"


let loop () =
  let env = Py.Module.get_function_with_keywords gym "make" [|(Py.String.of_string "CartPole-v1")|] ["render_mode", Py.String.of_string "human"] in
  let reset_fn = Py.Object.get_attr_string env "reset" in
  let step_fn = Py.Object.get_attr_string env "step" in


  let _result = Py.Callable.to_function (Core.Option.value_exn reset_fn) [||] in
  let rand_action () = Random.int 2 in

  let rec loop'(step: int)=
    let result = Py.Callable.to_function (Core.Option.value_exn step_fn) [|Py.Int.of_int @@ rand_action ()|] in
    let _state, _reward, _is_done, _truncated, _ = Py.Tuple.to_tuple5 result in
      if step mod 50 = 0 then
        let _ = Py.Callable.to_function (Core.Option.value_exn reset_fn) [||] in
        loop' (step+1)
      else 
          if step < 150 then
            loop' (step+1)
          else
          ()
    in
    loop' (0)





