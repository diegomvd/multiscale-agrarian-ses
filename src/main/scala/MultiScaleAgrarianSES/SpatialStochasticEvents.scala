package MultiScaleAgrarianSES

import scala.collection.immutable.ListMap

/**
 * Extends Landscapes by providing a function to select the spatial unit where a stochastic event is to take place.
 */
trait SpatialStochasticEvents:
  /**
   * Selects a unit of the landscape to realise a stochastic event according to a probability distribution and a random
   * number.
   *
   * @param x_rnd random number
   * @param prob  selection probability associated to each unit of the landscape
   * @return the VertexId of the selected unit
   * @todo now that I don't have vertexId, all the generic operations I was doing with the graphs might get more complex,
   *       maybe I should force every composition graph to have as node (id, unit) even if the unit also contains the id
   *       Any is actually of Unit type, maybe the thing would be to create a Unit trait englobing all the possible units
   * */
  def selectUnit(
                  x_rnd: Double,
                  prob: ListMap[(Long,LandscapeUnit),Double]
                ):
  LandscapeUnit =
    prob.find( x_rnd <= _._2 ) match
      case None => new LandscapeUnit{}
      case Some(((_,u),_)) => u

  def selectId(
                x_rnd: Double,
                prob: ListMap[(Long,LandscapeUnit), Double]
              ):
  Long =
    prob.find(x_rnd <= _._2) match
      case None => 0L
      case Some(((id,_),_)) => id

  def selectUnitAndId(
                       x_rnd: Double,
                       prob: ListMap[(Long, LandscapeUnit), Double]
                     ):
  (Long, LandscapeUnit) =
    prob.find(x_rnd <= _._2) match
      case None => (0L, new LandscapeUnit{}) 
      case Some((k, _)) => k


