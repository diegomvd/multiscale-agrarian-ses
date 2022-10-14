package MultiScaleAgrarianSES
import scala.math.pow
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
      case "static" => sim.runInitialization.outputStaticLandscapeOptimization(n)
      case "dynamic" => sim.runSocioEcoDynamics.outputStaticLandscapeOptimization(n)

  def parameters(
                  maximumSimulationTime: Double = 0.0,
                  ecoLandscapeRadius: Int = 30,
                  ecoConnectivityRange: Int = 1,
                  ecoServicesScalingExp: Double = 0.25,
                  ecoServicesScaleMax: Double = 0.25,
                  yEcoService: Double = 0.5,
                  sensRecovery: Double = 1.0,
                  sensDegradation: Double = 1.0,
                  sensFertilityLoss: Double = 1.0,
                  planningScale: Double = 0.5,
                  managementScale: Double = 0.5,
                  sensResourceDemand: Double = 1.0,
                  nHouseholdsSupportedPerHiIntUnit: Double = 10.0,
                  fractionOfMngUnitsSparing: Double = 0.5,
                  initFractionAgricultural: Double = 0.2,
                  initFractionDegraded: Double = 0.1
                ):
  Simulation =
    new Simulation(
      maximumSimulationTime,
      ecoLandscapeRadius,
      ecoConnectivityRange,
      ecoServicesScalingExp,
      ecoServicesScaleMax,
      yEcoService,
      sensRecovery,
      sensDegradation,
      sensFertilityLoss,
      planningScale,
      managementScale,
      sensResourceDemand,
      nHouseholdsSupportedPerHiIntUnit,
      fractionOfMngUnitsSparing,
      initFractionAgricultural,
      initFractionDegraded
    )

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)*pow(10.0,-9.0) + " s")
    result
  }


end Main


