package MultiScaleConservationPlanning

import MultiScaleAgrarianSES.{BaseLandscape, ModCo}
import MultiScaleAgrarianSES.EcoLandscape.buildStructure
import org.jgrapht.*
import org.jgrapht.graph.*
import org.jgrapht.alg.util.NeighborCache

import scala.util.Random

import scala.jdk.CollectionConverters.SetHasAsScala

case class EcoLandscape(
                         composition: Map[Long, EcoUnit],
                         structure: Graph[Long, DefaultEdge],
                         neighborCache: NeighborCache[Long, DefaultEdge],
                         size: Int,
                       )
extends BaseLandscape:

  type A = EcoUnit

  def updateProtectionStatus(
                              units: Vector[EcoUnit],
                              status: ProtectionStatus
                            ):
  EcoLandscape =
    EcoLandscape.updateProtectionStatus(this,units,status)

  def collectMostValuableUnits(
                                fractionProtected: Double
                              ):
  Vector[EcoUnit] =
    this.composition
      .map { case (_, eu) => eu }
      .toVector
      .sortWith(_.conservationValue > _.conservationValue)
      .take((fractionProtected * composition.size).toInt)

  def collectProtectedUnits:
  Vector[EcoUnit] =
    this.composition
      .collect{
        case (_, unit) if unit.status == ProtectionStatus.Protected => unit
      }
      .toVector

  def orderUnitsByConservationValue:
  Vector[EcoUnit] =
    this.composition.values.toVector.sortWith(_.conservationValue > _.conservationValue)


object EcoLandscape:

  def apply(
            r: Int,
            rnd: Random
           ):
  EcoLandscape =
    val comp = buildComposition(r,rnd)
    val struct = buildStructure(r, comp, 1)
    val neighborCache = new NeighborCache[Long,DefaultEdge](struct)
    val initializedComposition = initializeConservationValues(comp,struct,neighborCache)

    EcoLandscape(initializedComposition,struct,neighborCache,ModCo.area(r))

  def buildComposition(
                        r: Int,
                        rnd: Random
                      ):
  Map[Long, EcoUnit] =
    ModCo.apply(r).map { m => (m.toLong, EcoUnit(m.toLong, rnd.nextDouble, ProtectionStatus.UnProtected)) }.toMap

  private def initializeConservationValues(
                                composition: Map[Long,EcoUnit],
                                structure: Graph[Long, DefaultEdge],
                                neighborCache: NeighborCache[Long, DefaultEdge]
                              ):
  Map[Long,EcoUnit] =
    structure.vertexSet().asScala.toSet
      .map[(Long, EcoUnit)] {
        id => (id, composition.getOrElse(id, EcoUnit(-1L, 0.0, ProtectionStatus.UnProtected)).initializeConservationValue(composition, neighborCache))
      }.toMap

  def updateProtectionStatus(
                              ecoLandscape: EcoLandscape,
                              units: Vector[EcoUnit],
                              status: ProtectionStatus
                            ):
  EcoLandscape =
    val newComposition =
      ecoLandscape.composition
        .map{
          case (_,eu) => if units.contains(eu) then (eu.id, EcoUnit(eu.id, eu.conservationValue, status)) else (eu.id,eu)
        }
    EcoLandscape(newComposition,ecoLandscape.structure,ecoLandscape.neighborCache,ecoLandscape.size)

end EcoLandscape
