package MultiScaleAgrarianSES

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
 * Base implementation of the landscape trait. At their minimum expression landscapes are characterized by their
 * composition graph and their size.
 *
 * @note the size information is contained in the composition but it comes handy to stock it as a separate field to avoid
 * traversing the graph each time size information is needed. This is not costly because size is just an Int.
 */
trait Landscape[T]:

        val composition:Graph[T,UnDiEdge]
        val size:Int

        /**
         * Updates the composition of the landscape by changing a single unit.
         *
         * @param a is the VertexId of the unit to be changed
         * @param b is the new unit
         * @return an updated composition
         * @todo map functioning seems obscure, not sure if this preserves the edge
         * */
        def updateComposition[T:ClassTag](
                                           a:T,
                                           b:T
                                         ):
        Graph[T,UnDiEdge]=
                composition.map[T,UnDiEdge]{case node if node.value==a=>OuterNode(b)}

        /**
         * Updates the composition of the landscape by changing a collection of units.
         *
         * @param a is the collection of units to be changed
         * @param b is the new unit to replace all others
         * @return an updated composition
         * @todo How to preserve units' indexing in this method??!! This needs to be resolved, maybe I should ensure order
         *       with a list before calling, or with a key-value map
         * */
        def updateComposition[T:ClassTag](
                                           a:ParVector[T],
                                           b:ParVector[T]
                                         ):
        Graph[T,UnDiEdge]=
                composition.map[T,UnDiEdge]{case node if a.exists(_==node.value)=>b.find()}

