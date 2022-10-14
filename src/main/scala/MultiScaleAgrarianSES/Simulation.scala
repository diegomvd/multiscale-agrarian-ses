package MultiScaleAgrarianSES

class Simulation(
                  maximumSimulationTime: Double,
                  ecoLandscapeRadius: Int,
                  ecoConnectivityRange: Int,
                  ecoServicesScalingExp: Double,
                  ecoServicesScaleMax: Double,
                  yEcoService: Double,
                  sensRecovery: Double,
                  sensDegradation: Double,
                  sensFertilityLoss: Double,
                  planningScale: Double,
                  managementScale: Double,
                  sensResourceDemand: Double,
                  nHouseholdsSupportedPerHiIntUnit: Double,
                  fractionOfMngUnitsSparing: Double,
                  initFractionAgricultural: Double,
                  initFractionDegraded: Double
                ):

  def runSocioEcoDynamics:
  Matrix =
    runInitialization.simulate(this.maximumSimulationTime)

  def runInitialization:
  Matrix =
    println("Building the ecological landscape... ")
    val initEco = EcoLandscape(
      this.ecoLandscapeRadius,
      this.ecoConnectivityRange,
      this.ecoServicesScalingExp,
      this.ecoServicesScaleMax,
      this.yEcoService,
      this.sensRecovery,
      this.sensDegradation,
      this.sensFertilityLoss
    )
    println("Creating planning landscape... ")
    val plnLandscape = PlnLandscape(
      this.ecoLandscapeRadius,
      this.planningScale,
      initEco
    )
    println("Creating management landscape... ")
    val mngLandscape = MngLandscape(
      this.managementScale,
      plnLandscape,
      this.fractionOfMngUnitsSparing
    )
    println("Initializing ecological composition... ")
    val ecoLandscape = initEco.initialize(
      plnLandscape,
      mngLandscape,
      this.initFractionAgricultural,
      this.initFractionDegraded
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

end Simulation
