package MultiScaleAgrarianSES

import scala.collection.immutable.ListMap
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
Implementation of the Management Unit. MngUnits are the elementary constituents of the management landscape and are
composed of PlnUnits referenced through their VertexId in the PlnLandscape. MngUnits are also defined by their management
strategy: land-sparing or land-sharing.
@note Composition and strategy are determined at initialization and, for the time being, cannot be changed over the
      course of a simulation.
*/
case class MngUnit(
                    id: Long,
                    composition: Vector[Long],
                    strategy: MngStrategy
                  )
  extends LandscapeUnit:
  /**
  Determines if a MngUnit is available for conversion: at least one PlnUnit within the MngUnit must be available. Serves
  to build the probability of choosing any of the MngUnits of a MngLandscape: only available MngUnits can be chosen.
  @param pln the composition of the PlnLandscape that serves as base for the MngLandscape containing this MngUnit
  @param eco the composition of the EcoLandscape that serves as base for the PlnLandscape
  @return true if the unit is available, false if it isn't
  */
  def isAvailable(
                   pln: Map[Long,PlnUnit],
                   eco: Map[Long,EcoUnit]
                 ):
  Boolean =
    val subLandscape = PlnLandscape.subLandscape(pln,this.composition)
    subLandscape.exists(_._2.isAvailable(eco))

  /**
  Calculates the conversion propensity for each of the PlnUnits belonging to this MngUnit given this MngUnit's conversion
  propensity an initial propensity value and this MngUnit's strategy.
  @param i_val the initial value for the cumulative sum of the propensities
  @param u_tcp this MngUnit's total conversion propensity
  @param pln the PlnLandscape containing the PlnUnits from this MngUnit
  @param eco is the composition of the EcoLandscape serving as base for the PlnLandscape
  @return a ListMap with the cumulative propensities for each PlnUnit inside the MngUnit
  */
  def propensityOfPlnUnits(
                            i_val: Double,
                            u_tcp: Double,
                            pln: PlnLandscape,
                            eco: Map[Long,EcoUnit]
                          ):
  ListMap[Long,Double] =
    // Calculate the individual propensities, sort them by id and store them in a ListMap
    val prop: ListMap[Long,Double] =
      ListMap(
        MngUnit.weights(this.composition,pln,eco,this.strategy)
          .map{
            case (id,p) => (id, p * u_tcp)
          }
          .toSeq.sortWith(_._1 < _._1):_*
      )
    // Cumulative sum starting with the initial value and yielding the cumulative propensities scaled to the rest of the world's propensities
    prop.scanLeft[(Long,Double)]((-1L,i_val)){
      (pre, now) => (now._1, now._2 + pre._2)
    }.tail.to(ListMap)

object MngUnit :

  def apply():
  MngUnit =
    MngUnit(-1L,Vector(),MngStrategy.LandSharing)
  /**
  @param comp the composition of this MngUnit
  @param pln the PlnLandscape containing the PlnUnits from this MngUnit
  @param eco the composition of the EcoLandscape that serves as base for the PlnLandscape
  @param stg is thi MngUnit's management strategy
  @return a VertexRDD with the relative conversion probability associated to each PlnUnit of this MngUnit
  */
  def weights(
               comp: Vector[Long],
               pln: PlnLandscape,
               eco: Map[Long,EcoUnit],
               stg: MngStrategy
             ):
  Map[Long,Double] =

    def normalize(
                   v: Map[Long,Double]
                 ):
    Map[Long,Double]=
      // normally w should be strictly positive since this calculation would be never
      // done in an unavailable unit, this is being extra cautious and can be helpful for debugging
      val v_tot: Double = v.foldLeft[Double](0.0)( (sum, map) => sum + map._2)
      if v_tot > 0.0 then v.map{ case (id,p) => (id,p/v_tot) } else v

    // land-sparing units prefer to segregate new conversions from natural land
    // thus weighting more available units close to unavailable ones
    // land-sharing units prefer to integrate new conversions with natural land
    // thus weighting more available units close to available ones
    stg match
      case MngStrategy.LandSparing =>
        normalize( pln.unavailableNeighbors(comp,eco)
          .map{
            case (id,nn) => (id, PlnUnit.weightExpression(nn, MngStrategy.LandSparing.clustering))
          }
        )
      case MngStrategy.LandSharing =>
        normalize( pln.availableNeighbors(comp,eco)
          .map{
            case (id,nn) => (id, PlnUnit.weightExpression(nn, MngStrategy.LandSharing.clustering))
          }
        )

end MngUnit
