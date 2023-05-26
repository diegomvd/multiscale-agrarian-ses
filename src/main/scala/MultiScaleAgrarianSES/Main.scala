package MultiScaleAgrarianSES
import scala.math.pow
import scala.util.Random
/**
 * Still need to write the output function that selects outputs from a Matrix Instance
 * */

object Main extends App:

  private val simulation = parameters()
  //val res = time { main(simulation,"static",1) }
  //println(res)

  //val res = time {
  //  main(simulation, 1)
  //}

  // Social-ecological dynamics
  val res = time {
    val world = simulation.runSocialEcoDynamics
    println(world.historicNaturalArea)
    //println(world.stateVariability(world.historicNaturalArea,20,10))
    println(world.stateVariability(world.historicNaturalArea,20,10).max)
    println(world.stateVariability(world.historicNaturalArea,20,10).sum/world.stateVariability(world.historicNaturalArea,20,10).size.toDouble)
  }
  //println(res)


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


  def main(
            sim: Simulation,
            rStat: Int,
            targetCover: LandCover
          ):
  Double =
    sim.runSpatialStatistics(rStat).outputSpatialStatisticsNoDiagram(targetCover)

  def main(
            sim:Simulation,
            rStat: Int
          ):
  (Double,Double,Double) =
    sim.runSpatialStatistics(rStat).outputSpatialStatisticsESProd

  def parameters(
                  maximumSimulationTime: Double = 400.0,
                  ecoLandscapeRadius: Int = 10,
                  ecoConnectivityRadius: Int = 1, //0.058
                  ecoServicesScalingExp: Double = 0.25,// 0.25,
                  ecoServicesMaxArea: Double = 1.0,
                  yEcoService: Double = 0.3,
                  sensRecovery: Double = 1.0,
                  sensDegradation: Double = 0.2,
                  sensFertilityLoss: Double = 0.4,
                  planningArea: Int = 1,
                  managementArea: Double = 1.0,//0.0031,//1.0,
                  sensResourceDemand: Double = 50.0,
                  nHouseholdsSupportedPerHiIntUnit: Double = 10.0,
                  fractionOfMngUnitsSparing: Double = 0.0,
                  initFractionAgricultural: Double = 0.2,
                  initFractionDegraded: Double = 0.0,
                  seed: Long = 127L
                ):
  Simulation =
    new Simulation(
      maximumSimulationTime,
      ecoLandscapeRadius,
      ecoConnectivityRadius,
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


