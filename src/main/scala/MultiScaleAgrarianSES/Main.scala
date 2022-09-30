package MultiScaleAgrarianSES

/**
 * Still need to write the output function that selects outputs from a Matrix Instance
 * */

object Main:

  def main(
            sim: Simulation,
            simulationType: String
          ):
  Matrix =
    simulationType match
      case "static" => sim.runInitialization
      case "dynamic" => sim.runSocioEcoDynamics

  def parameters(maximumSimulationTime: Double = 0.0,
                 ecoLandscapeRadius: Int = 10,
                 ecoConnectivityRange: Int = 1,
                 ecoServicesScalingExp: Double = 0.25,
                 yEcoService: Double = 0.5,
                 sensRecovery: Double = 1.0,
                 sensDegradation: Double = 1.0,
                 sensFertilityLoss: Double = 1.0,
                 planningScale: Double = 1.0,
                 managementScale: Double = 1.0,
                 sensResourceDemand: Double = 1.0,
                 nHouseholdsSupportedPerHiIntUnit: Double = 1.0,
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


end Main


