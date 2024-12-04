module Make (_ : Simulation.Config) : sig
  include Simulation.S

  val random_between : float -> float -> float

end
