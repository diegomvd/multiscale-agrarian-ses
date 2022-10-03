package MultiScaleAgrarianSES

class Simulation(
                  maximumSimulationTime: Double,
                  ecoLandscapeRadius: Int,
                  ecoConnectivityRange: Int,
                  ecoServicesScalingExp: Double,
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
    val initEco = EcoLandscape(
      this.ecoLandscapeRadius,
      this.ecoConnectivityRange,
      this.ecoServicesScalingExp,
      this.yEcoService,
      this.sensRecovery,
      this.sensDegradation,
      this.sensFertilityLoss
    )
    val plnLandscape = PlnLandscape(
      this.ecoLandscapeRadius,
      this.planningScale,
      initEco
    )
    val mngLandscape = MngLandscape(
      this.managementScale,
      plnLandscape,
      this.fractionOfMngUnitsSparing
    )
    val ecoLandscape = initEco.initialize(
      plnLandscape,
      mngLandscape,
      this.initFractionAgricultural,
      this.initFractionDegraded
    )
    val humanPop = HumanPop(
      ecoLandscape.resourceProduction(ecoLandscape.ecoServices),
      this.sensResourceDemand,
      this.nHouseholdsSupportedPerHiIntUnit
    )
    Matrix(
      ecoLandscape,
      plnLandscape,
      mngLandscape,
      humanPop
    )

end Simulation
