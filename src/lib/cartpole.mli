module Make (_ : Simulation.Config) : sig
  include Simulation.S

  val simulate : t -> unit

end