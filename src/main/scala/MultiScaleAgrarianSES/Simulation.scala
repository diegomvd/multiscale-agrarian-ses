package MultiScaleAgrarianSES

import scala.util.Random

case class Simulation(
                  maximumSimulationTime: Double,
                  ecoLandscapeRadius: Int,
                  ecoConnectivityArea: Double,
                  ecoServicesScalingExp: Double,
                  ecoServicesMaxArea: Double,
                  yEcoService: Double,
                  sensRecovery: Double,
                  sensDegradation: Double,
                  sensFertilityLoss: Double,
                  planningArea: Double,
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
    runInitialization.simulate(this.maximumSimulationTime)

  def runInitialization:
  Matrix =
    //println("Building the ecological landscape... ")
    val initEco = EcoLandscape(
      this.ecoLandscapeRadius,
      this.ecoConnectivityArea,
      this.ecoServicesScalingExp,
      this.ecoServicesMaxArea,
      this.yEcoService,
      this.sensRecovery,
      this.sensDegradation,
      this.sensFertilityLoss
    )
    //println("Creating planning landscape on top... ")
    val plnLandscape = PlnLandscape(
      this.ecoLandscapeRadius,
      this.planningArea,
      initEco,
      this.random
    )
    //println("Creating management landscape no top... ")
    val mngLandscape = MngLandscape(
      ecoLandscapeRadius,
      this.managementArea,
      plnLandscape,
      this.fractionOfMngUnitsSparing,
      this.random
    )
    //println("Initializing ecological composition... ")
    val ecoLandscape = initEco.initialize(
      plnLandscape,
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
      plnLandscape,
      mngLandscape,
      humanPop,
      this.random
    )

object Simulation:

  /**
   * Constructor for static landscape optimization
   * */
  def apply(
             ecoLandscapeRadius: Int,
             ecoConnectivityArea: Double,
             ecoServicesScalingExp: Double,
             ecoServicesMaxArea: Double,
             yEcoService: Double,
             planningArea: Double,
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
      ecoConnectivityArea,
      ecoServicesScalingExp,
      ecoServicesMaxArea,
      yEcoService,
      sensRecovery = 1.0,
      sensDegradation = 1.0,
      sensFertilityLoss = 1.0,
      planningArea,
      managementArea,
      sensResourceDemand = 1.0,
      nHouseholdsSupportedPerHiIntUnit,
      fractionOfMngUnitsSparing,
      initFractionAgricultural,
      initFractionDegraded,
      Random(seed)
    )

end Simulation
