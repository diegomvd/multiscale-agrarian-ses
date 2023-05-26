package MultiScaleConservationPlanning

case class Experiment(
                      simulation: Simulation
                     ):

  def replications(n: Int):
  Vector[Simulation] =
    Vector.fill(n)(this.simulation)



