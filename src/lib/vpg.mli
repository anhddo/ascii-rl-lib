open Base_algorithm

module Make : functor (_ : Base_algorithm.Algo_config) (_ : Simulation.S) -> sig
  include Algo_base
end