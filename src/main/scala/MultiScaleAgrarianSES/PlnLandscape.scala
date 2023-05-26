package MultiScaleAgrarianSES

import scala.math.pow
import scala.math.max
import scala.util.Random

import org.jgrapht.*
import org.jgrapht.alg.util.NeighborCache
import org.jgrapht.graph.*
import scala.jdk.CollectionConverters.SetHasAsScala


/**
Implementation of the Planning Landscape. A PlnLandscape is on the top of an EcoLandscape and at the base of a
MngLandscape. It is composed by a collection of PlnUnits.

@author diego

@todo I am not sure that adjacency is determined correctly in the voronoi tesselation because neighborhood in the
      EcoLandscape is functional connectivity rather than adjacency
*/
case class PlnLandscape(
                         composition: Map[Long,PlnUnit],
                         structure: Graph[Long,DefaultEdge],
                         neighborCache: NeighborCache[Long,DefaultEdge],
                         unitArea: Int,
                         size: Int
                       )
  extends TopLandscape with BaseLandscape with SpatialStochasticEvents:
    type A = PlnUnit
    /**
     * Calculates the number of available neighbors of each PlnUnit in the selected MngUnit. Used to determine the
     * PlnUnits conversion weights in a land-sharing MngUnit.
     * @param mng_unit the MngUnit's composition: a region of this PlnLandscape for which we want to determine neighbor
     *                 availability.
     * @param eco the composition of the EcoLandscape at the base of this PlnLandscape.
     * @return a VertexRDD with the number of available neighbors.
    */
    def availableNeighbors(
                            mng_unit: Vector[Long],
                            eco: Map[Long,EcoUnit]
                          ):
    Map[Long, Int] =
      PlnLandscape.neighborAvailability(this.subLandscape(mng_unit),this.structure,this.neighborCache,eco,true)

    /**
     * Calculates the number of unavailable neighbors of each PlnUnit in the selected MngUnit. Used to determine the
     * PlnUnits conversion weights in a land-sparing MngUnit.
     * @param mng_unit the MngUnit's composition: a region of this PlnLandscape for which we want to determine neighbor
     *                 availability.
     * @param eco the composition of the EcoLandscape at the base of this PlnLandscape.
     * @return a VertexRDD with the number of available neighbors.
     */
    def unavailableNeighbors(
                              mng_unit: Vector[Long],
                              eco: Map[Long,EcoUnit]
                            ):
    Map[Long,Int] =
      PlnLandscape.neighborAvailability(this.subLandscape(mng_unit),this.structure,this.neighborCache,eco,false)

    /**
     * Selects a part of this PlnLandscape corresponding to the composition of a MngUnit.
     * @param mng_unit the MngUnit's composition: a region of this PlnLandscape for which we want to determine neighbor
     *                 availability.
     * @return a sub graph of this PlnLandscape's composition.
    */
    def subLandscape(
                      mng_unit: Vector[Long]
                    ):
    Map[Long,PlnUnit] =
      this.composition.filter(c => mng_unit.contains(c._1))

object PlnLandscape :
  /**
   * Builds the composition of a PlnLandscape from the base landscape and a number of PlnUnits to be created.
   * @param nu the number of planning units to create
   * @param eco the EcoLandscape at the base of the to be created PlnLandscape
   * @return a PlnLandscape composition graph.
   * @todo need to check these functions depending on tesselation. Update to scala-graph still uncertain, need to work out
   *       the voronoi tesselation first
  */
  def buildCompositionAndStructure(
                                    nu: Int,
                                    eco: EcoLandscape,
                                    rnd: Random
                                  ):
  (Map[Long,PlnUnit], Graph[Long,DefaultEdge]) =
    val (compInit, struct): (Map[Long,Vector[Long]], Graph[Long,DefaultEdge]) = eco.tesselate(nu,rnd)
    (compInit.map{case (id,vec) => (id,PlnUnit(id,vec))}, struct)

  /**
   * Builds a PlnLandscape given its base landscape and its relative scale
   * @constructor
   * @param scale the relative scale of the PlnLandscape to the EcoLandscape.
   * @param eco the EcoLandscape at the base of the PlnLandscape.
   * @return a PlnLandscape.
   * */
  def apply(
             r: Int,
             unitArea: Int,
             eco: EcoLandscape,
             rnd: Random
           ):
  PlnLandscape =
    // Transform relative unit area to absolute unit area
    //val unitAreaAbs: Int = (unitArea * ModCo.area(r).toDouble).toInt
    val unitAreaAbs: Int = unitArea
    val adjacencyNeighborhoodEcoLandscape: EcoLandscape = eco.copy(structure = EcoLandscape.buildStructure(r,eco.composition,1))
    val nu = TopLandscape.numberOfUnits(unitAreaAbs,adjacencyNeighborhoodEcoLandscape.size)
    val (comp,struct) = buildCompositionAndStructure(nu,adjacencyNeighborhoodEcoLandscape,rnd)
    val neighborCache: NeighborCache[Long,DefaultEdge] = new NeighborCache[Long,DefaultEdge](struct)
    PlnLandscape(comp,struct,neighborCache,unitAreaAbs,nu)

  /**
   * Determines the un/availability of the neighbors of each PlnUnit in the selected MngUnit.
   * @param comp the composition graph of the part of the PlnLandscape corresponding to the selected MngUnit.
   * @param eco the composition of the EcoLandscape at the base of this PlnLandscape.
   * @param available specifies if the function should look for available or unavailable neighbors.
   * @return the number of available/unavailable neighbors for each available unit.
   * @todo must check if there is need to send 0 when dstAttr is not available
  */
  def neighborAvailability(
                            comp: Map[Long,PlnUnit],
                            struct: Graph[Long,DefaultEdge],
                            neighborCache: NeighborCache[Long,DefaultEdge],
                            eco: Map[Long,EcoUnit],
                            available: Boolean
                          ):
  Map[Long, Int] =
    struct.vertexSet().asScala.toSet.collect{
      case n if comp.getOrElse(n, PlnUnit()).isAvailable(eco) =>
        if available then (n, neighborCache.neighborsOf(n).asScala.toSet.count(x => comp.getOrElse(x, PlnUnit()).isAvailable(eco)))
        else (n, neighborCache.neighborsOf(n).asScala.toSet.count(x => !comp.getOrElse(x, PlnUnit()).isAvailable(eco)))
    }.toMap

  def subLandscape(
                    composition: Map[Long,PlnUnit],
                    mng_unit: Vector[Long]
                  ):
  Map[Long, PlnUnit] =
    composition.filter(c => mng_unit.contains(c._1))

end PlnLandscape
