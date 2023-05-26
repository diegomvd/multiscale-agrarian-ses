package MultiScaleConservationPlanning

import MultiScaleAgrarianSES.{ModCo, TopLandscape}
import org.jgrapht.*
import org.jgrapht.graph.*
import org.jgrapht.alg.util.NeighborCache

import scala.annotation.tailrec
import scala.util.Random

case class MngLandscape(composition: Map[Long, MngUnit],
                        structure: Graph[Long, DefaultEdge],
                        unitArea: Int,
                        protectedFraction: Double,
                        size: Int)
extends TopLandscape:

  type A = MngUnit

  def createProtectedAreas(
                            ecoLandscape: EcoLandscape,
                          ):
  EcoLandscape =
    ecoLandscape.updateProtectionStatus(
      MngLandscape.getEcoUnitsToProtect(ecoLandscape.composition,this.composition,this.protectedFraction),
      ProtectionStatus.Protected
    )


object MngLandscape:
  def apply(
             ecoLandscape: EcoLandscape,
             administrativeRegions: Int,
             protectedFraction: Double,
             rnd: Random
           ):
  MngLandscape =

    val (compInit, struct): (Map[Long, Vector[Long]], Graph[Long, DefaultEdge]) = ecoLandscape.tesselate(administrativeRegions, rnd)

    val comp = compInit.map {
      case (id, vec) =>
        (id, MngUnit(id, vec))
    }
    MngLandscape(comp, struct, ecoLandscape.composition.size/administrativeRegions, protectedFraction, administrativeRegions)

  private def getEcoUnitsToProtect(
                                    ecoLandscapeComposition: Map[Long, EcoUnit],
                                    mngLandscapeComposition: Map[Long, MngUnit],
                                    protectedFraction: Double
                                  ):
  Vector[EcoUnit] =

    mngLandscapeComposition.flatMap {
      case (_, mu) => mu.takeMostValuableEcoUnits(ecoLandscapeComposition, protectedFraction)
    }.toVector






end MngLandscape

