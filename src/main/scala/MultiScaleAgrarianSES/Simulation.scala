package MultiScaleAgrarianSES

import scala.util.Random

case class Simulation(
                  maximumSimulationTime: Double,
                  ecoLandscapeRadius: Int,
                  ecoServicesScalingExp: Double,
                  yEcoService: Double,
                  sensRecovery: Double,
                  sensDegradation: Double,
                  sensFertilityLoss: Double,
                  managementArea: Double,
                  sensResourceDemand: Double,
                  nHouseholdsSupportedPerHiIntUnit: Double,
                  fractionOfMngUnitsSparing: Double,
                  initFractionAgricultural: Double,
                  initFractionDegraded: Double,
                  random: Random
                ):


  def runSocialEcoDynamics:
  Matrix =
    runInitialization.simulate(maxT = this.maximumSimulationTime,0.1)

  def runInitialization:
  Matrix =
    //println("Building the ecological landscape... ")
    val initEco = EcoLandscape(
      this.ecoLandscapeRadius,
      this.ecoServicesScalingExp,
      this.yEcoService,
      this.sensRecovery,
      this.sensDegradation,
      this.sensFertilityLoss
    )
    
    //println("Creating management landscape no top... ")
    val mngLandscape = MngLandscape(
      ecoLandscapeRadius,
      this.managementArea,
      initEco,
      this.fractionOfMngUnitsSparing,
      this.random
    )
    //println("Initializing ecological composition... ")
    val ecoLandscape = initEco.initialize(
      mngLandscape,
      this.initFractionAgricultural,
      this.initFractionDegraded,
      this.random
    )

    //println("Initializing human population at equilibrium with resources... ")
    val humanPop = HumanPop(
      ecoLandscape.resourceProduction(ecoLandscape.ecoServices),
      this.sensResourceDemand,
      this.nHouseholdsSupportedPerHiIntUnit
    )
    
    // println("Initializing the Matrix... ")
    Matrix(
      ecoLandscape,
      mngLandscape,
      humanPop,
      this.random
    )

  def runSpatialStatistics(rStat: Int):
  Matrix =
    val initEco = EcoLandscape(
      this.ecoLandscapeRadius,
      rStat,
      1, // @todo: make this connectivity radius a a parameter in the simulation noo tin the model
      this.ecoServicesScalingExp,
      this.yEcoService,
      this.sensRecovery,
      this.sensDegradation,
      this.sensFertilityLoss
    )
    
    val mngLandscape = MngLandscape(
      ecoLandscapeRadius,
      this.managementArea,
      initEco,
      this.fractionOfMngUnitsSparing,
      this.random
    )

    val ecoLandscape = initEco.initialize(
      mngLandscape,
      this.initFractionAgricultural,
      this.initFractionDegraded,
      this.random
    )

    Matrix(
      ecoLandscape,
      mngLandscape,
      HumanPop(0.0,0.0,0.0),
      this.random
    )

object Simulation:

  /**
   * Constructor for static landscape optimization
   * */
  def apply(
             ecoLandscapeRadius: Int,
             ecoServicesScalingExp: Double,
             yEcoService: Double,
             managementArea: Double,
             nHouseholdsSupportedPerHiIntUnit: Double,
             fractionOfMngUnitsSparing: Double,
             initFractionAgricultural: Double,
             initFractionDegraded: Double,
             seed: Long
           ):
  Simulation =
    Simulation(
      maximumSimulationTime = 0.0,
      ecoLandscapeRadius,
      ecoServicesScalingExp,
      yEcoService,
      sensRecovery = 1.0,
      sensDegradation = 1.0,
      sensFertilityLoss = 1.0,
      managementArea,
      sensResourceDemand = 1.0,
      nHouseholdsSupportedPerHiIntUnit,
      fractionOfMngUnitsSparing,
      initFractionAgricultural,
      initFractionDegraded,
      Random(seed)
    )

end Simulation
