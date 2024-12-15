open Base_algorithm

module Make : functor (_ : Algo_config) (_ : Simulation.S) -> sig
  include Algo_base
end