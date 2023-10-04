package MultiScaleEcoNetConservation

import MultiScaleAgrarianSES.{EcoUnit, TopLandscape}
import org.jgrapht.Graph
import org.jgrapht.graph.DefaultEdge

import scala.util.Random

case class MngLandscape(composition: Map[Long, MngUnit],
                        structure: Graph[Long, DefaultEdge],
                        unitArea: Int,
                        totalProtection: Double,
                        protectionHeterogeneity: Double,
                        size: Int)
  extends TopLandscape:

  type A = MngUnit

  /*def degradationProbability(
                              ecoUnit: EcoUnit
                            ):
  Double =
    composition.collectFirst(
      (_, u) => if u.composition.contains(ecoUnit.id) then 1.0-u.protectionLevel
    )*/
object MngLandscape:

  //def apply(
       //      ecoLandscape: EcoLandscape,
     //        administrativeRegions: Int,
   //          protectedFrac: Double,
  //           rnd: Random
  //         ):
  //MngLandscape =

    //val (compInit, struct): (Map[Long, Vector[Long]], Graph[Long, DefaultEdge]) = ecoLandscape.tesselate(administrativeRegions, rnd)

    //val comp = compInit.map {
    //  case (id, vec) =>
    //    (id, MngUnit(id, vec))
    //}
    //MngLandscape(comp, struct, ecoLandscape.composition.size / administrativeRegions, protectedFraction, administrativeRegions)

end MngLandscape
