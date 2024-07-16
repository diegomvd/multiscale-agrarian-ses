package MultiScaleAgrarianSES

import scala.collection.immutable.ListMap
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.alg.util.NeighborCache

import scala.jdk.CollectionConverters.SetHasAsScala


/**
Implementation of the Management Unit. MngUnits are the elementary constituents of the management landscape and are
composed of EcoUnits referenced through their VertexId in the MngLandscape. MngUnits are also defined by their management
strategy: land-sparing or land-sharing.
@note Composition and strategy are determined at initialization and, for the time being, cannot be changed over the
      course of a simulation.
 
 @todo: many functions could be moved to the EcoLandscape class
*/
case class MngUnit(
                    id: Long,
                    composition: Vector[Long],
                    strategy: MngStrategy
                  )
  extends LandscapeUnit:

  /**
  Determines if a MngUnit is available for conversion: at least one EcoUnit must be convertible (i.e in a Natural state). Serves
  to build the probability of choosing any of the MngUnits of a MngLandscape: only available MngUnits can be chosen.
  @param eco the composition of the EcoLandscape that serves as base for the MngLandscape
  @return true if the unit is available, false if it isn't
  */
  def isAvailable(
                   eco: Map[Long,EcoUnit]
                 ):
  Boolean =
    // Get all the eco units inside this management unit
    val ecoUnits: Vector[EcoUnit] = eco.collect {
      case (id, u) if composition.contains(id) => u
    }.toVector
    // Check if there is at least one natural unit that can be converted
    val predicate1 = ecoUnits.exists {
      _.matchCover(LandCover.Natural)
    }
    predicate1

  /**
   * Determines the un/availability of the neighbors of each EcoUnit in the selected MngUnit.
   *
   * @param eco         the composition of the EcoLandscape at the base of this MngLandscape.
   * @param available   specifies if the function should look for available or unavailable neighbors.
   * @return the number of available/unavailable neighbors for each available unit.
   */
  def neighborAvailability(
                            structEco: Graph[Long, DefaultEdge],
                            neighborCache: NeighborCache[Long, DefaultEdge],
                            eco: Map[Long, EcoUnit],
                            available: Boolean
                          ):
  Map[Long, Int] =

    val subLandscapeEco: Map[Long,EcoUnit] = MngUnit.subLandscape(eco,composition)
    structEco.vertexSet().asScala.toSet.collect {
      case n if subLandscapeEco.getOrElse(n, EcoUnit()).matchCover(LandCover.Natural) =>
        if available then (n, neighborCache.neighborsOf(n).asScala.toSet.count(x => eco.getOrElse(x, EcoUnit()).matchCover(LandCover.Natural)))
        else (n, neighborCache.neighborsOf(n).asScala.toSet.count(x => !eco.getOrElse(x, EcoUnit()).matchCover(LandCover.Natural)))
    }.toMap

  /**
   * @param eco  the composition of the EcoLandscape that serves as base for the MngLandscape
   * @param stg  is thi MngUnit's management strategy
   * @return a Map with the relative conversion probability associated to each EcoUnit of this MngUnit
   */
  private def weights(
               structEco: Graph[Long, DefaultEdge],
               neighborCache: NeighborCache[Long, DefaultEdge],
               eco: Map[Long, EcoUnit],
               stg: MngStrategy
             ):
  Map[Long, Double] =

    def normalize(
                   v: Map[Long, Double]
                 ):
    Map[Long, Double] =
      // normally w should be strictly positive since this calculation would be never
      // done in an unavailable unit, this is being extra cautious and can be helpful for debugging
      val v_tot: Double = v.foldLeft[Double](0.0)((sum, map) => sum + map._2)
      if v_tot > 0.0 then v.map { case (id, p) => (id, p / v_tot) } else v

    // land-sparing units prefer to segregate new conversions from natural land
    // thus weighting more available units close to unavailable ones
    // land-sharing units prefer to integrate new conversions with natural land
    // thus weighting more available units close to available ones
    stg match
      case MngStrategy.LandSparing =>
        normalize(this.neighborAvailability(structEco, neighborCache, eco, false)
          .map {
            case (id, nn) => (id, EcoUnit.weightExpression(nn, MngStrategy.LandSparing.clustering))
          }
        )
      case MngStrategy.LandSharing =>
        normalize(this.neighborAvailability(structEco, neighborCache, eco, true)
          .map {
            case (id, nn) => (id, EcoUnit.weightExpression(nn, MngStrategy.LandSharing.clustering))
          }
        )

  /**
  Calculates the conversion propensity for each of the EcoUnits belonging to this MngUnit given this MngUnit's conversion
  propensity an initial propensity value and this MngUnit's strategy.
  @param i_val the initial value for the cumulative sum of the propensities
  @param u_tcp this MngUnit's total conversion propensity
  @param eco is the composition of the EcoLandscape serving as base for the MngLandscape
  @return a ListMap with the cumulative propensities for each EcoUnit inside the MngUnit
  */
  def propensityOfEcoUnits(
                            structEco: Graph[Long, DefaultEdge],
                            neighborCache: NeighborCache[Long, DefaultEdge],
                            i_val: Double,
                            u_tcp: Double,
                            eco: Map[Long,EcoUnit]
                          ):
  ListMap[Long,Double] =
    // Calculate the individual propensities, sort them by id and store them in a ListMap
    val prop: ListMap[Long,Double] =
      ListMap(
        this.weights(structEco,neighborCache,eco,this.strategy)
          .map{
            case (id,p) =>  (id, p * u_tcp)
          }
          .toSeq.sortWith(_._1 < _._1)*
      )
    // Cumulative sum starting with the initial value and yielding the cumulative propensities scaled to the rest of the world's propensities
    prop.scanLeft[(Long,Double)]((-1L,i_val)){
      (pre, now) => (now._1, now._2 + pre._2)
    }.tail.to(ListMap)

object MngUnit :

  def apply():
  MngUnit =
    MngUnit(-1L,Vector(),MngStrategy.LandSharing)

  def subLandscape(
                    composition: Map[Long, EcoUnit],
                    mng_unit: Vector[Long]
                  ):
  Map[Long, EcoUnit] =
    composition.filter(c => mng_unit.contains(c._1))


end MngUnit
