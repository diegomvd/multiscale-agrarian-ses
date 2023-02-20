package MultiScaleAgrarianSES
import scala.math.pow
import scala.util.Random
/**
 * Still need to write the output function that selects outputs from a Matrix Instance
 * */

object Main extends App:

  val simulation = parameters()
  val res = time { main(simulation,"static",1) }
  println(res)

  /**
   * Function to return EcoServices average and robustness and population size
   * */
  def main(
            sim: Simulation,
            simulationType: String,
            n: Int
          ):
  (Double,Double,Int) =
    simulationType match
      case "static" =>
       // println("Preparing static landscape configuration simulation...")
        sim.runInitialization.outputStaticLandscapeOptimization(n)
      case "dynamic" =>
       // println("Preparing social-ecological dynamics simulation...")
        sim.runSocialEcoDynamics.outputStaticLandscapeOptimization(n)

  def parameters(
                  maximumSimulationTime: Double = 0.0,
                  ecoLandscapeRadius: Int = 10,
                  ecoConnectivityArea: Double = 0.04, //0.058
                  ecoServicesScalingExp: Double = 0.25,
                  ecoServicesMaxArea: Double = 1.0,
                  yEcoService: Double = 0.5,
                  sensRecovery: Double = 1.0,
                  sensDegradation: Double = 1.0,
                  sensFertilityLoss: Double = 1.0,
                  planningArea: Double = 0.0035,
                  managementArea: Double = 1.0,
                  sensResourceDemand: Double = 1.0,
                  nHouseholdsSupportedPerHiIntUnit: Double = 1.0,
                  fractionOfMngUnitsSparing: Double = 1.0,
                  initFractionAgricultural: Double = 0.7,
                  initFractionDegraded: Double = 0.0,
                  seed: Long = 123456L
                ):
  Simulation =
    new Simulation(
      maximumSimulationTime,
      ecoLandscapeRadius,
      ecoConnectivityArea,
      ecoServicesScalingExp,
      ecoServicesMaxArea,
      yEcoService,
      sensRecovery,
      sensDegradation,
      sensFertilityLoss,
      planningArea,
      managementArea,
      sensResourceDemand,
      nHouseholdsSupportedPerHiIntUnit,
      fractionOfMngUnitsSparing,
      initFractionAgricultural,
      initFractionDegraded,
      Random(seed)
    )

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)*pow(10.0,-9.0) + " s")
    result
  }


end Main


