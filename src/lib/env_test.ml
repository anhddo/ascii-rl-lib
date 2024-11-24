[@@@ocaml.warning "-32"]

module type Config = sig
    val name : string
    val render : bool
   end


module Make =
functor
  (M : Config)
  ->
  struct
    (* if not initialize then initialized*)
    if not (Py.is_initialized ()) then Py.initialize ()

    let gym = Py.import "gymnasium"

    let init_environment (str : string) (render : bool) =
      if render then
        Py.Module.get_function_with_keywords gym "make"
          [| Py.String.of_string str |]
          [ ("render_mode", Py.String.of_string "human") ]
      else Py.Module.get_function gym "make" [| Py.String.of_string str |]

    let env = init_environment M.name M.render
  end

module Config1: Config = struct
    let name = "CartPole-v1"
    let render = false
  end
module Env1 = Make(Config1)