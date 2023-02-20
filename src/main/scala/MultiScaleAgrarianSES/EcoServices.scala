package MultiScaleAgrarianSES

import scala.util.Random
import scala.math.pow
import scala.annotation.tailrec
import scala.reflect.*
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.jdk.CollectionConverters.MutableSetHasAsJava

import org.jgrapht.*
import org.jgrapht.graph.*
import org.jgrapht.alg.connectivity.BiconnectivityInspector
import org.jgrapht.alg.util.NeighborCache

import java.util.function.Predicate


/**
Extends a landscape composed by a graph of EcoUnits with ecosystem services functionalities. The trait serves to
retrieve a graph of ecosystem services flow and the natural connected components as a metric of fragmentation. All the
intermediate functions are located in the companion object.
 */
trait EcoServices :

  val size: Int
  val scal_exp: Double
  val area_max: Int
  val composition: Map[Long,EcoUnit]
  val structure: Graph[Long,DefaultEdge]
  val neighborCache: NeighborCache[Long,DefaultEdge] // to avoid re-calculation of neighborhood at each time.

  /**
   * @return a map with the EcoUnit as key and their incoming ES flow as value
   * */
  def ecoServices:
  Map[Long, Double]  =
    EcoServices.ecoServices(this.structure,this.neighborCache,this.composition,this.scal_exp,this.size,area_max.toDouble/this.size)

  /**
   *  @return the set of disconnected natural connected components
   */
  def naturalConnectedComponents:
  Map[Long, Graph[Long, DefaultEdge]] =
    EcoServices.naturalConnectedComponents(this.structure,this.composition)

  def averageEcoServices:
  Double =
    val es = this.ecoServices
    es.values.sum/es.size.toDouble

  def robustnessEcoServicesOneReplica(
                                       average: Double,
                                       rnd: Random
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
        val nId: Long = rnd.shuffle(comp.filter(_._2.matchCover(LandCover.Natural)).keys).take(1).head
        val newComp: Map[Long, EcoUnit] = comp.map { v => if v._1 == nId then (v._1, EcoUnit(nId, LandCover.Degraded)) else v }
        val newAverage: Double = EcoServices.averageEcoServices(this.structure, this.neighborCache, newComp, this.scal_exp, this.size, area_max.toDouble)
        rec(threshold, newAverage, newComp, new_n)
    val threshold: Double = average * 0.5
    rec(threshold, average, this.composition, 0) / this.composition.count(_._2.matchCover(LandCover.Natural)).toDouble

  def robustnessEcoServices(
                             average: Double,
                             n: Int,
                             rnd: Random
                           ):
  Double =
    (0 until n).map(_ => robustnessEcoServicesOneReplica(average,rnd)).sum / n.toDouble

  def averageAndRobustnessEcoServices(
                                       n: Int,
                                       rnd: Random
                                     ):
  (Double,Double) =
    val avg : Double = this.averageEcoServices
    (avg,this.robustnessEcoServices(avg,n,rnd))

object EcoServices :

  def naturalConnectedComponents(
                                  struct: Graph[Long,DefaultEdge],
                                  comp: Map[Long,EcoUnit]
                                ):
  Map[Long, Graph[Long, DefaultEdge]] =
    val naturalUnits: java.util.Set[Long] = struct.vertexSet().asScala.filter( comp.getOrElse(_,EcoUnit()).matchCover(LandCover.Natural)).asJava
    val naturalLandscape: Graph[Long,DefaultEdge] = new AsSubgraph[Long,DefaultEdge](struct,naturalUnits)
    val naturalConnectivity: BiconnectivityInspector[Long,DefaultEdge] = new BiconnectivityInspector[Long,DefaultEdge](naturalLandscape)
    val ncc = naturalConnectivity.getConnectedComponents.asScala.toSet
      .zipWithIndex
      .map(_.swap)
      .map { case (nccId, g) => (nccId.toLong, g) }
      .toMap
    //val max_size: Double = ncc.map( m =>  m._2.vertexSet().size ).toList.max.toDouble/comp.size.toDouble
    ncc

  /**
   * Creates a Map with EcoUnits as keys and NCC id as value.
   *
   * @param ncc the map of natural connected components
   * @return the ecounit-ncc map
   * */
  def nodeComponentMembership(
                               ncc: Map[Long, Graph[Long, DefaultEdge]]
                             ):
  Map[Long, Long] =
    ncc.flatMap{
      case (id, graph) => graph.vertexSet().asScala.toSet.map(node => (node, id))
    }

  /**
   * Creates a Map with the NCC id as key and its area in number of nodes as value
   * @param ncc the map of natural connected components
   * @return the map ncc-area
   * */
  def nccNormalizedAreaMap(
                            ncc: Map[Long, Graph[Long, DefaultEdge]],
                            size: Double
                          ):
  Map[Long,Double] =
    ncc.map{
      case (id, graph) => (id, graph.vertexSet().asScala.toSet.size.toDouble / size)
    }

  /**
   *  @param a is the area of the natural component
   *  @param z is the scaling exponent of the ecosystem services area relationship
   *  @return the value of ecosystem service provision for a component of area a
   */
  def esAreaRelation(
                      a: Double,
                      z: Double,
                      a_max: Double
                    ):
  Double =
    if a > a_max then 1.0 else pow(a, z)

  def outgoingEcoServicePerUnit(
                                 ncm: Map[Long,Long],
                                 nam: Map[Long,Double],
                                 scaling_exp: Double,
                                 a_max: Double
                               ):
  Map[Long, Double] =
    ncm.map{
      case (node, nccId) => (node, esAreaRelation( nam.getOrElse(nccId, 0.0), scaling_exp,a_max) )
    }

  /**
   * Calculates the ecosystem service
   * @todo double check the function*/
  def incomingEcoServicePerUnit(
                                 struct: Graph[Long, DefaultEdge],
                                 neighborCache: NeighborCache[Long,DefaultEdge],
                                 out: Map[Long,Double]
                               ):
  Map[Long, Double] =
    struct.vertexSet().asScala.toSet
      .map[(Long, Double)]{
        n =>
          val (count,es) =
            neighborCache.neighborsOf(n).asScala.toSet.foldLeft[(Int, Double)]((0, 0.0)) { // (neighbor count, eco services)
              case ((count, es), neighbor) => (count + 1, es + out.getOrElse(neighbor, 0.0)) 
            }
          //if es>0.0 then println(es/count.toDouble)
          ( n, es/count.toDouble )
     }.toMap

  def ecoServices(
                   struct: Graph[Long,DefaultEdge],
                   neighborCache: NeighborCache[Long,DefaultEdge],
                   comp: Map[Long,EcoUnit],
                   scal_exp: Double,
                   size: Int,
                   a_max: Double
                 ):
  Map[Long,Double] =
    val ncc = EcoServices.naturalConnectedComponents(struct, comp)
    val ncm = EcoServices.nodeComponentMembership(ncc)
    val nam = EcoServices.nccNormalizedAreaMap(ncc, size.toDouble)
    val out = EcoServices.outgoingEcoServicePerUnit(ncm, nam, scal_exp, a_max/size.toDouble)
    EcoServices.incomingEcoServicePerUnit(struct, neighborCache, out)

  def averageEcoServices(
                          struct: Graph[Long, DefaultEdge],
                          neighborCache: NeighborCache[Long,DefaultEdge],
                          comp: Map[Long, EcoUnit],
                          scal_exp: Double,
                          size: Int,
                          a_max: Double
                        ):
  Double =
    val es = ecoServices(struct,neighborCache,comp,scal_exp,size,a_max)
    es.values.sum / es.size.toDouble

end EcoServices


