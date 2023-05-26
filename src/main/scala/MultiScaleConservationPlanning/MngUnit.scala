package MultiScaleConservationPlanning

import MultiScaleAgrarianSES.LandscapeUnit

import scala.collection.immutable.ListMap

case class MngUnit(
                    id: Long,
                    composition: Vector[Long]
                  )
  extends LandscapeUnit:

    def takeMostValuableEcoUnits(
                                  ecoLandscapeComposition: Map[Long,EcoUnit],
                                  fraction: Double
                                ):
    Vector[EcoUnit] =
      MngUnit.takeMostValuableEcoUnits(this.composition,ecoLandscapeComposition,fraction)
object MngUnit:

  def maximumConservationValue(
                                mngUnitComposition: Vector[Long],
                                ecoLandscapeComposition: Map[Long,EcoUnit]
                              ):
  Double =
    ecoLandscapeComposition
      .collect{ case (_,eu) if mngUnitComposition.contains(eu.id) => eu.conservationValue }
      .max

  def takeMostValuableEcoUnits(
                                mngUnitComposition: Vector[Long],
                                ecoLandscapeComposition: Map[Long,EcoUnit],
                                fraction: Double):
  Vector[EcoUnit] =
    ecoLandscapeComposition
      .collect{ case (_,eu) if mngUnitComposition.contains(eu.id) => eu }
      .toVector
      .sortWith(_.conservationValue>_.conservationValue)
      .take((fraction*mngUnitComposition.size).toInt)



end MngUnit




