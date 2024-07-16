package MultiScaleAgrarianSES

import scala.collection.immutable.ListMap
import scala.util.Random
import scala.reflect.*
import org.jgrapht.*
import org.jgrapht.graph.*

import scala.annotation.tailrec

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
                         structure: Graph[Long,DefaultEdge],
                         unitArea: Int,
                         n_sparing: Int,
                         size: Int)
  extends TopLandscape with SpatialStochasticEvents:
  type A = MngUnit
  /**
   * Calculates the propensity of choosing each MngUnit for agricultural expansion.
   * @param i_val initial value for the cumulative sum of conversion propensities in each MngUnit
   * @param tcp total conversion propensity determined by resource demand
   * @return a ListMap containing cumulative propensity for choosing each management unit
  */
  def propensityOfMngUnits(
                            i_val: Double,
                            tcp: Double,
                            eco: Map[Long,EcoUnit]
                          ):
  ListMap[Long,Double] =
    val propensities: Map[Long,Double] = MngLandscape.probabilities(this.composition,eco,tcp)
    propensities.scanLeft((-1L, i_val))( (pre, now) => (now._1, now._2 + pre._2)).tail.to(ListMap)

object MngLandscape :
  /**
   * MngLandscape constructor
 *
   * @constructor
   * @param averageUnitArea the average area of a management unit relative to the eco landscape area.
   * @param fs the fraction of land-sparing MngUnits
   * @return an instance of MngLandscape
   *
   */
  @tailrec
  def apply(
             ecoRadius: Int,
             averageUnitArea: Double,
             eco: EcoLandscape,
             fs: Double,
             rnd: Random
           ):
  MngLandscape =
    // Transform relative management area to absolute
    val unitAreaAbs : Int = (averageUnitArea * ModCo.area(ecoRadius).toDouble).toInt
    val nm = TopLandscape.numberOfUnits(unitAreaAbs,ModCo.area(ecoRadius))
    val (compInit, struct): (Map[Long,Vector[Long]], Graph[Long,DefaultEdge]) = eco.tesselate(nm,rnd)
    val n_sparing: Int = (fs * nm).toInt
    val sparing_ids: Vector[Long] = rnd.shuffle(compInit.keys).take( n_sparing ).toVector
    val comp = compInit.map{
      case (id,vec) =>
        if sparing_ids.contains(id)
        then (id,MngUnit(id,vec,MngStrategy.LandSparing))
        else (id,MngUnit(id,vec,MngStrategy.LandSharing))
    }
    MngLandscape(comp,struct,unitAreaAbs,n_sparing,nm)

  /**
   *  Calculate the relative probabilities for each MngUnit to be selected for a conversion event.
   * @param mng the management landscape's composition
   * @param eco the ecological landscape's composition
   * @return an RDD with the relative conversion probabilities of each management unit
   * @note At the current modeling stage MngUnits are selected with uniform probability
  */
  private def probabilities(
                     mng: Map[Long,MngUnit],
                     eco: Map[Long,EcoUnit],
                     tcP: Double
                   ):
  Map[Long,Double] =
    val available_units = mng.filter( _._2.isAvailable(eco) )
    available_units.map{ case (id,_) => (id, 1.0/available_units.size*tcP) }

end MngLandscape
