package MultiScaleAgrarianSES

import scala.collection.immutable.ListMap
import scala.util.Random as rnd
import scala.reflect._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
Implementation of the Management Landscape, composed by Management Units. A MngLandscape is extends a TopLandscape and
is thus defined by its composition, size and scale. Additionally the class has a "nsparing" parameter that represents
the number of Management Units f the Management Landscape that apply a land-sparing strategy.
@note The Management Landscape is instantiated at initialization and, for the time being, changes in the Management
      Landscape during a simulation are not implemented.
@author diego
*/
case class MngLandscape(
                         composition: Map[Long,MngUnit],
                         structure: Graph[Long,UnDiEdge],
                         scale: Double,
                         n_sparing: Int,
                         size: Int)
  extends TopLandscape with SpatialStochasticEvents:
  /**
   * Calculates the propensity of choosing each MngUnit for agricultural expansion.
   * @param ival initial value for the cumulative sum of conversion propensities in each MngUnit
   * @param tcp total conversion propensity determined by resource demand
   * @return a ListMap containing cumulative propensity for choosing each management unit
  */
  def propensityOfMngUnits(
                            i_val: Double,
                            tcp: Double,
                            pln: Map[Long,PlnUnit],
                            eco: Map[Long,EcoUnit]
                          ):
  ListMap[Long,Double] =
    val propensities: Map[Long,Double] = MngLandscape.probabilities(this.composition,pln,eco)
    propensities.scanLeft((-1L, i_val))( (pre, now) => (now._1, now._2 + pre._2)).to(ListMap)

object MngLandscape :
  /**
   * MngLandscape constructor
   * @constructor
   * @param scale the relative scale of this management landscape to the planning landscape
   * @param pln the planning landscape serving as base for this management landscape
   * @param fs the fraction of land-sparing MngUnits
   * @return an instance of MngLandscape
   *
   * @todo need to check this function depending on tesselation
   */
  def apply(
             scale: Double,
             pln: PlnLandscape,
             fs: Double
           ):
  MngLandscape =
    val nm = TopLandscape.numberOfUnits(scale,pln.size)
    val (compInit, struct): (Map[Long,Vector[Long]], Graph[Long,UnDiEdge]) = pln.tesselate(nm)
    val n_sparing = fs * nm
    val sparing_ids: Vector[Long] = rnd.shuffle(compInit.keys).take( (fs * n_sparing).toInt ).toVector
    val comp = compInit.map{
      case (id,vec) =>
        if sparing_ids.contains(id)
        then (id,MngUnit(id,vec,MngStrategy.LandSparing))
        else (id,MngUnit(id,vec,MngStrategy.LandSparing))
    }
    MngLandscape(comp,struct,scale,n_sparing.toInt,nm)

  /**
   *  Calculate the relative probabilities for each MngUnit to be selected for a conversion event.
   * @param mng the management landscape's composition
   * @param pln the planning landscape's composition
   * @param eco the ecological landscape's composition
   * @return an RDD with the relative conversion probabilities of each management unit
   * @note At the current modeling stage MngUnits are selected with uniform probability
  */
  def probabilities(
                     mng: Map[Long,MngUnit],
                     pln: Map[Long,PlnUnit],
                     eco: Map[Long,EcoUnit]
                   ):
  Map[Long,Double] =
    val available_units = mng.filter( _._2.isAvailable(pln,eco) )
    available_units.map{ case (id,_) => (id, 1.0/available_units.size) }

end MngLandscape
