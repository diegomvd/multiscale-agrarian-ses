package MultiScaleAgrarianSES

import scala.math.pow
import scala.annotation.tailrec
import scala.reflect._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
Extends a landscape composed by a graph of EcoUnits with ecosystem services functionalities. The trait serves to
retrieve a graph of ecosystem services flow and the natural connected components as a metric of fragmentation. All the
intermediate functions are located in the companion object.
 */
trait EcoServices :

  val size: Int
  val scal_exp: Double
  val composition: Map[Long,EcoUnit]
  val structure: Graph[Long,UnDiEdge]

  /**
   * @return a map with the EcoUnit as key and their incoming ES flow as value
   * */
  def ecoServices:
  Map[Long, Double]  =
    EcoServices.ecoServices(this.structure,this.composition,this.scal_exp,this.size)

  /**
   *  @return the set of disconnected natural connected components
   */
  def naturalConnectedComponents:
  Map[Long, Graph[Long, UnDiEdge]] =
    EcoServices.naturalConnectedComponents(this.structure,this.composition)

  def averageEcoServices:
  Double =
    val es = this.ecoServices
    es.values.sum/es.size.toDouble

  def robustnessEcoServicesOneReplica(
                                       average: Double,
                                     ):
  Double =
    @tailrec
    def rec(
             threshold: Double,
             currentAvg: Double,
             comp: Map[Long, EcoUnit],
             n: Int
           ):
    Double =
      if currentAvg <= threshold then n.toDouble
      else
        val new_n: Int = n + 1
        val nId: Long = rnd.shuffle(comp.filter(_._2.matchCover(LandCover.Natural)).keys).take(1)
        val newComp: Map[Long, Double] = comp.map { case (id, _) if id == nId => (id, EcoUnit(id, LandCover.Degraded)) }
        val newAverage: Double = EcoServices.averageEcoServices(this.structure, newComp, this.scal_exp, this.size)
        rec(threshold, newAverage, newComp, this.scal_exp, this.size, new_n)
    val threshold: Double = average * 0.5
    rec(threshold, average, this.composition, this.scal_exp, this.size, 0) / this.composition.count(_._2.matchCover(LandCover.Natural)).toDouble

  def robustnessEcoServices(
                             average: Double,
                             n: Int
                           ):
  Double =
    (0 until n).flatMap(_ => robustnessEcoServicesOneReplica(average)).reduceLeft((a, b) => a + b) / n

  def averageAndRobustnessEcoServices(
                                       n: Int
                                     ):
  (Double,Double) =
    (this.averageEcoServices,this.robustnessEcoServices(n))

object EcoServices :

  def naturalConnectedComponents(
                                  struct: Graph[Long,UnDiEdge],
                                  comp: Map[Long,EcoUnit]
                                ):
  Map[Long, Graph[Long, UnDiEdge]] =
    struct.componentTraverser().withSubgraph(n => comp.getOrElse(n.toOuter, EcoUnit()).matchCover(LandCover.Natural)) // Get components of the natural subgraph
      .map(_.to(Graph)) // Cast the components to graphs
      .zipWithIndex // Associate each component with an index
      .map(_.swap) // Invert order of indices and graphs
      .map { case (nccId, g) => (nccId.toLong, g) }.toMap // Convert the index to Long type

  /**
   * Creates a Map with EcoUnits as keys and NCC id as value.
   *
   * @param ncc the map of natural connected components
   * @return the ecounit-ncc map
   * */
  def nodeComponentMembership(
                               ncc: Map[Long, Graph[Long, UnDiEdge]]
                             ):
  Map[Long, Long] =
    ncc.flatMap{
      case (id, graph) => graph.nodes.toOuter.map(node => (node, id))
    }

  /**
   * Creates a Map with the NCC id as key and its area in number of nodes as value
   * @param ncc the map of natural connected components
   * @return the map ncc-area
   * */
  def nccNormalizedAreaMap(
                            ncc: Map[Long, Graph[Long, UnDiEdge]],
                            size: Double
                          ):
  Map[Long,Double] =
    ncc.map{
      case (id, graph) => (id, graph.nodes.size.toDouble / size)
    }

  /**
   *  @param a is the area of the natural component
   *  @param z is the scaling exponent of the ecosystem services area relationship
   *  @return the value of ecosystem service provision for a component of area a
   */
  def esAreaRelation(
                      a: Double,
                      z: Double
                    ):
  Double =
    pow(a, z)

  def outgoingEcoServicePerUnit(
                                 ncm: Map[Long,Long],
                                 nam: Map[Long,Double],
                                 scaling_exp: Double
                               ):
  Map[Long, Double] =
    ncm.map{
      case (node, nccId) => (node, esAreaRelation( nam.getOrElse(nccId, 0.0), scaling_exp) )
    }

  /**
   * Calculates the ecosystem service */
  def incomingEcoServicePerUnit(
                                 struct: Graph[Long, UnDiEdge],
                                 out: Map[Long,Double]
                               ):
  Map[Long, Double] =
    (struct get struct.nodes.head).innerNodeTraverser.map[(Long, Double)]{
      n =>
        val (count,es) =
          n.neighbors.foldLeft[(Int, Double)]((0, 0.0)) { // (neighbor count, eco services)
            case ((count, es), neighbor) => (count + 1, es + out.getOrElse(neighbor.toOuter, 0.0))
          }
        ( n.toOuter, es/count.toDouble )
    }.toMap

  def ecoServices(
                   struct: Graph[Long,UnDiEdge],
                   comp: Map[Long,EcoUnit],
                   scal_exp: Double,
                   size: Int
                 ):
  Map[Long,Double] =
    val ncc = EcoServices.naturalConnectedComponents(struct, comp)
    val ncm = EcoServices.nodeComponentMembership(ncc)
    val nam = EcoServices.nccNormalizedAreaMap(ncc, size.toDouble)
    val out = EcoServices.outgoingEcoServicePerUnit(ncm, nam, scal_exp)
    EcoServices.incomingEcoServicePerUnit(struct, out)

  def averageEcoServices(
                          struct: Graph[Long, UnDiEdge],
                          comp: Map[Long, EcoUnit],
                          scal_exp: Double,
                          size: Int
                        ):
  Double =
    val es = ecoServices(struct,comp,scal_exp,size)
    es.values.sum / es.size.toDouble

end EcoServices


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////



*/

