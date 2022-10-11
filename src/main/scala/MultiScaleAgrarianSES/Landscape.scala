package MultiScaleAgrarianSES

import scalax.collection.Graph
import scalax.collection.GraphPredef.*
import scalax.collection.GraphEdge.*

/**
 * Base implementation of the landscape trait. At their minimum expression landscapes are characterized by their
 * composition map, their structure graph and their size.
 *
 * @note the size information is contained in the composition but it comes handy to stock it as a separate field to avoid
 * traversing the graph each time size information is needed. This is not costly because size is just an Int.
 */
trait Landscape:
  type A <: LandscapeUnit
  val composition: Map[Long,A]
  val structure: Graph[Long,UnDiEdge]
  val size:Int

  /**
   * Updates the composition of the landscape by changing a single unit.
   *
   * @param a is the Id of the unit to be updated
   * @param b is the new unit
   * @return an updated composition
   * */
  def updateComposition(
                         a:Long,
                         b:A
                       ):
  Map[Long,A]=
    this.composition.map(
      v => if v._1 == a then (v._1,b) else v
    )

  /**
   * Updates the composition of the landscape by changing a collection of units.
   *
   * @param a is the collection of Ids of the units to be changed
   * @param b is the new unit to replace all others
   * @return an updated composition
   * @note finding the corresponding unit can be done with an option pattern and with a map instead of vector
   * */
  def updateComposition(
                         a:Vector[Long],
                         b:Vector[A]
                       ):
  Map[Long,A]=
    this.composition.map(
      v =>
        if a.contains(v._1)
        then {
          (v._1, b.apply( b.indexWhere(_.id == v._1)))
        }
        else v
   )

