package MultiScaleAgrarianSES

import scala.util.Random

case class Simulation(
                  maximumSimulationTime: Double,
                  ecoLandscapeRadius: Int,
                  ecoConnectivityRange: Int,
                  ecoServicesScalingExp: Double,
                  ecoServicesMaxRadius: Int,
                  yEcoService: Double,
                  sensRecovery: Double,
                  sensDegradation: Double,
                  sensFertilityLoss: Double,
                  planningRadius: Int,
                  managementRadius: Int,
                  sensResourceDemand: Double,
                  nHouseholdsSupportedPerHiIntUnit: Double,
                  fractionOfMngUnitsSparing: Double,
                  initFractionAgricultural: Double,
                  initFractionDegraded: Double,
                  random: Random
                ):

  def runSocioEcoDynamics(rnd:Random):
  Matrix =
    runInitialization.simulate(this.maximumSimulationTime,rnd)

  def runInitialization:
  Matrix =
    println("Building the ecological landscape... ")
    val initEco = EcoLandscape(
      this.ecoLandscapeRadius,
      this.ecoConnectivityRange,
      this.ecoServicesScalingExp,
      this.ecoServicesMaxRadius,
      this.yEcoService,
      this.sensRecovery,
      this.sensDegradation,
      this.sensFertilityLoss
    )
    println("Creating planning landscape... ")
    val plnLandscape = PlnLandscape(
      this.ecoLandscapeRadius,
      this.planningRadius,
      initEco,
      this.random
    )
    println("Creating management landscape... ")
    val mngLandscape = MngLandscape(
      this.managementRadius,
      plnLandscape,
      this.fractionOfMngUnitsSparing,
      this.random
    )
    println("Initializing ecological composition... ")
    val ecoLandscape = initEco.initialize(
      plnLandscape,
      mngLandscape,
      this.initFractionAgricultural,
      this.initFractionDegraded,
      this.random
    )
    println("Initializing human population at equilibrium with resources... ")
    val humanPop = HumanPop(
      ecoLandscape.resourceProduction(ecoLandscape.ecoServices),
      this.sensResourceDemand,
      this.nHouseholdsSupportedPerHiIntUnit
    )
    println("Initializing the Matrix... ")
    Matrix(
      ecoLandscape,
      plnLandscape,
      mngLandscape,
      humanPop
    )

object Simulation:

  /**
   * Constructor for static landscape optimization
   * */
  def apply(
             ecoLandscapeRadius: Int,
             ecoConnectivityRange: Int,
             ecoServicesScalingExp: Double,
             ecoServicesMaxRadius: Int,
             yEcoService: Double,
             planningRadius: Int,
             managementRadius: Int,
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
      ecoConnectivityRange,
      ecoServicesScalingExp,
      ecoServicesMaxRadius,
      yEcoService,
      sensRecovery = 1.0,
      sensDegradation = 1.0,
      sensFertilityLoss = 1.0,
      planningRadius,
      managementRadius,
      sensResourceDemand = 1.0,
      nHouseholdsSupportedPerHiIntUnit,
      fractionOfMngUnitsSparing,
      initFractionAgricultural,
      initFractionDegraded,
      Random(seed)
    )

end Simulation
