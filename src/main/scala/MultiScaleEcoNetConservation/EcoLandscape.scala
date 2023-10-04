/*package MultiScaleEcoNetConservation

import MultiScaleAgrarianSES.{BaseLandscape, EcoUnit, LandCover, ModCo, SpatialStochasticEvents}
import MultiScaleAgrarianSES.EcoLandscape.buildStructure
import MultiScaleConservationPlanning.EcoLandscape.initializeConservationValues
import org.jgrapht.Graph
import org.jgrapht.alg.util.NeighborCache
import org.jgrapht.graph.DefaultEdge

import scala.annotation.tailrec
import scala.util.Random

case class EcoLandscape(
                        composition: Map[Long, EcoUnit],
                        structure: Graph[Long, DefaultEdge],
                        neighborCache: NeighborCache[Long, DefaultEdge],
                        fractionDegraded: Double,
                        size: Int
                       )
extends BaseLandscape with SpatialStochasticEvents:

  type A = EcoUnit

  def initialize(
                  mngLandscape: MngLandscape,
                  rnd: Random
                ):
  EcoLandscape =
    val newComposition = initializeComposition(this.composition,mngLandscape,rnd)
    EcoLandscape(newComposition)

  private def initializeComposition(
                            mngLandscape: MngLandscape,
                            rnd: Random
                           ):
  Map[Long,EcoUnit] =

    @tailrec
    def rec(
            comp: Map[Long,EcoUnit]
           ):
    Map[Long,EcoUnit] =
      if this.countDegraded >= fractionDegraded*this.size then comp
      else{
        val degradationProb = EcoLandscape.degradationProbabilities(comp,mngLandscape)
        val unitId = this.selectUnitId(rnd.nextDouble(),degradationProb)
        val ecoUnit = EcoUnit(unitId,LandCover.Degraded)
        val newComp = this.updateComposition(ecoUnit)
        rec(newComp)
      }

    rec(this.composition)

  def updateComposition(unit: EcoUnit):
  Map[Long,EcoUnit] =
    newComposition = this.composition.map {
      case (_, u) => if u == unit then (_, unit) else (_, u)
    }

  def countDegraded: Int = this.composition.count{ case (_,u) => u.matchCover(LandCover.Degraded) }

object EcoLandscape:

  def apply(
             r: Int,
             fractionDegraded: Double
           ):
  EcoLandscape =
    val comp = ModCo.apply(r).map { m => (m.toLong, EcoUnit(m.toLong, LandCover.Natural )) }.toMap
    val struct = buildStructure(r, comp, 1)
    val neighborCache = new NeighborCache[Long,DefaultEdge](struct)
    EcoLandscape(comp,struct,neighborCache,fractionDegraded,ModCo.area(r))

  private def degradationProbabilities(
                              comp: Map[Long,EcoUnit],
                              mng: MngLandscape,
                            ):
  ListMap[Long,Double] =
    comp.collect{
      case (_,u) =>
        if u.matchCover(LandCover.Natural) then mng.degradationProbability(u)
    }.scanLeft( (-1L, 0.0) ){
      (pre,now) => (now._1, now._2 + pre._2)
    }.tail.to(ListMap)


end EcoLandscape
*/