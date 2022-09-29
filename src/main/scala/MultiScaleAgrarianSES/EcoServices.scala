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
    val ncc = this.naturalConnectedComponents
    val ncm = EcoServices.nodeComponentMembership(ncc)
    val nam = EcoServices.nccNormalizedAreaMap(ncc,this.size.toDouble)
    val out = EcoServices.outgoingEcoServicePerUnit(ncm,nam,this.scal_exp)
    EcoServices.incomingEcoServicePerUnit(this.structure,out)

  /**
   *  @return the set of disconnected natural connected components
   */
  def naturalConnectedComponents: Map[Long, Graph[Long, UnDiEdge]] =
    this.structure.componentTraverser().withSubgraph(n => this.composition.getOrElse(n.toOuter,EcoUnit()).matchCover(LandCover.Natural)) // Get components of the natural subgraph
      .map(_.to(Graph)) // Cast the components to graphs
      .zipWithIndex // Associate each component with an index
      .map(_.swap) // Invert order of indices and graphs
      .map{case (nccId,g) => (nccId.toLong,g)}.toMap // Convert the index to Long type

object EcoServices :

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

end EcoServices


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
 /** def averageESFlow(
    eco: Graph[EcoUnit,Long],
    z: Double,
    size: Int):
    Double =
      esGraph(eco,z,size).vertices.reduce{ case ((v, a),(_,b)) => (v, a._2 + b._2) }._2 / size.toDouble 


  /**
  Fraction of natural habitat that needs to be removed with uniform probability to
  halve average ecosystem service provision
  */
  def robustnessESFlowOneReplica(average: Double,
                                 eco: Graph[EcoUnit,Long],
                                 z: Double,
                                 size: Int): Double = {

      val thr: Double = average * 0.5
      val vid: VertexId = rnd.shuffle( eco.subgraph(vpred = (_,eu) => eu.cover == Natural).vertices.collect() ).take(1)._1
      val new_eco: Graph[EcoUnit,Long] = eco.mapValues{ case (v,_) if v == vid => (v, Degraded) }
      val new_avg: Double = averageESFlow(new_eco,z,size)
      val n: Int = 1

      //@tailrec
      def rec(thr: Double,
              current_avg: Double,
              eco: Graph[EcoUnit,Long],
              z: Double,
              size: Int,
              n: Int): Double = {
        if(current_avg <= thr) { n }
        else {
          val new_n: Int = n + 1
          val vid: VertexId = rnd.shuffle( eco.subgraph(vpred = (_,eu) => eu.cover == Natural).vertices.collect() ).take(1)._1
          val new_eco: Graph[EcoUnit,Long] = eco.mapValues{ case (v,_) if v == vid => (v, Degraded) }
          val new_avg: Double = averageESFlow(new_eco,z,size)
          rec(thr,new_avg,new_eco,z,size,new_n)
        }
      }
      rec(thr,new_avg,new_eco,z,size,n) / eco.subgraph(vpred = (_,eu) => eu.cover == Natural).vertices.count.toInt
  }

  def robustnessESFlow(average: Double,
                       eco: Graph[EcoUnit,Long],
                       z: Double,
                       size: Int,
                       n: Int): Double = {
    (0 until n).flatMap( _ => robustnessESFlowOneReplica(average,eco,z,size) ).reduce((a,b) => a + b)/n
  }
*/

