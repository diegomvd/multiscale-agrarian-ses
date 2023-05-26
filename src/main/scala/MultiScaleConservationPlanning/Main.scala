package MultiScaleConservationPlanning

import scala.math.pow
import scala.util.Random

object Main extends App:

  private val simulation = parameters()

  val res = time {
    main(simulation)
  }
  println(res)


  def main(
            simulation: Simulation
          ):
  Double =
    simulation.runRealizedVersusPotentialConservationValue

  def parameters(
                  landscapeRadius: Int = 10,
                  administrativeRegions: Int = 128,
                  protectedFraction: Double = 0.3,
                  seed: Long = 1L
                ):
  Simulation =
    new Simulation(
      landscapeRadius,
      administrativeRegions,
      protectedFraction,
      Random(seed)
    )

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) * pow(10.0, -9.0) + " s")
    result
  }

end Main

