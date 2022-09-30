package MultiScaleAgrarianSES

import scala.collection.immutable.ListMap

/**
 * Extends Landscapes by providing a function to select the spatial unit where a stochastic event is to take place.
 */
trait SpatialStochasticEvents:

  def selectUnitIdWithPropensity(
                                  x_rnd: Double,
                                  prob: ListMap[Long, Double]
                                ):
  (Long, Double) =
    prob.find(x_rnd <= _._2) match
      case None => (-1L, 0.0)
      case Some(x) => x

  def selectUnitId(
                    x_rnd: Double,
                    prob: ListMap[Long, Double]
                  ):
  Long =
    prob.find(x_rnd <= _._2) match
      case None => -1L
      case Some(x) => x._1

