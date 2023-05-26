package MultiScaleConservationPlanning

import MultiScaleAgrarianSES.LandscapeUnit
import org.jgrapht.alg.util.NeighborCache
import org.jgrapht.graph.DefaultEdge

import scala.jdk.CollectionConverters.SetHasAsScala

case class EcoUnit(
                    id: Long,
                    conservationValue: Double,
                    status: ProtectionStatus
                  )
extends LandscapeUnit:
  def initializeConservationValue(
                                   composition: Map[Long, EcoUnit],
                                   neighborCache: NeighborCache[Long, DefaultEdge]
                                 ):
  EcoUnit =
    val (count, value) =
      neighborCache.neighborsOf(this.id).asScala.toSet.foldLeft[(Int, Double)]((0, 0.0)) {
        case ((count, value), neighbor) => (count + 1, value + composition.getOrElse(neighbor, EcoUnit(-1L, 0.0, ProtectionStatus.UnProtected)).conservationValue)
      }
    EcoUnit(this.id, value/count.toDouble, ProtectionStatus.UnProtected)

object EcoUnit:



end EcoUnit




