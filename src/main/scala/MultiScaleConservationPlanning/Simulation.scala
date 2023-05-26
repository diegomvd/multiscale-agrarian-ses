package MultiScaleConservationPlanning

import scala.util.Random

case class Simulation(
                        landscapeRadius: Int,
                        administrativeRegions: Int,
                        protectedFraction: Double,
                        rnd: Random
                     ):

  def runFractionOfGlobalMostValuableProtected:
  Double =
    World(landscapeRadius,administrativeRegions,protectedFraction,rnd).fractionOfGlobalMostValuableProtected

  def runUnitStatusRankedByValue:
  Vector[String] =
    World(landscapeRadius,administrativeRegions,protectedFraction,rnd).unitStatusRankedByValue

  def runRealizedVersusPotentialConservationValue:
  Double =
    World(landscapeRadius, administrativeRegions, protectedFraction, rnd).realizedVersusPotentialConservationValue

object Simulation:

  def apply(seed: Long):
  Simulation =
    Simulation(
      landscapeRadius = 100,
      administrativeRegions = 1,
      protectedFraction = 0.3,
      rnd = Random(seed)
    )

  def apply(
             landscapeRadius: Int,
             administrativeRegions: Int,
             protectedFraction: Double,
             seed: Long
           ):
  Simulation =
    Simulation(
      landscapeRadius,
      administrativeRegions,
      protectedFraction,
      rnd = Random(seed)
    )

  def apply(
             landscapeRadius: Int,
             administrativeRegions: Int,
             protectedFraction: Double
           ):
  Simulation =
    Simulation(
      landscapeRadius,
      administrativeRegions,
      protectedFraction,
      rnd = Random()
    )

end Simulation
