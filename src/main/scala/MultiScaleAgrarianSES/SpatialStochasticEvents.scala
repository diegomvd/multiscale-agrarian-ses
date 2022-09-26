package MultiScaleAgrarianSES

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
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
         * */
        def selectVId(
        x_rnd:Double,
        prob:ListMap[Any,Double]):
        Any={
                prob.find(x_rnd<=_._2)match{
                        case None=>null //TODO: I should warn that None should not happen
                        case Some(x)=>x._1
                }
        }

