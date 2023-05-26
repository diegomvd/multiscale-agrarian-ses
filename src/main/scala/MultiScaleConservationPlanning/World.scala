package MultiScaleConservationPlanning

import scala.util.Random

case class World(
                  ecoLandscape: EcoLandscape,
                  mngLandscape: MngLandscape
                ):
  def fractionOfGlobalMostValuableProtected:
  Double =
    val mvu = ecoLandscape.collectMostValuableUnits(this.mngLandscape.protectedFraction)
    mvu.collect{
        case unit if ecoLandscape.collectProtectedUnits.contains(unit) => 1.0
    }.sum/mvu.size

  def unitStatusRankedByValue:
  Vector[String] =
    this.ecoLandscape.orderUnitsByConservationValue.map{
      unit => if unit.status == ProtectionStatus.Protected then "Protected" else "Unprotected"
    }

  def realizedVersusPotentialConservationValue:
  Double =
    val pcv: Double = ecoLandscape.collectMostValuableUnits(this.mngLandscape.protectedFraction).map(eu => eu.conservationValue).sum
    println(pcv)
    val rcv: Double = ecoLandscape.composition.values.collect{ case eu if eu.status == ProtectionStatus.Protected => eu.conservationValue}.sum
    println(rcv)
    rcv/pcv


object World:
  def apply(
             radius: Int,
             administrativeRegions: Int,
             fractionProtected: Double,
             rnd: Random
           ):
  World =
    val initEco = EcoLandscape(radius,rnd)
    val mng = MngLandscape(initEco,administrativeRegions,fractionProtected,rnd)
    val eco = mng.createProtectedAreas(initEco)
    World(eco,mng)

end World

