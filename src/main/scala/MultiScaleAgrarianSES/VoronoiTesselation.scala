package MultiScaleAgrarianSES

import scala.collection.immutable.ListMap
import scala.annotation.tailrec
import scala.util.Random
import scala.jdk.CollectionConverters.ListHasAsScala

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.Graphs.neighborListOf

/**
 * Used to extend a Landscape to a BaseLandscape. This trait gives a Landscape the possibility to perform a Voronoi
 * tesselation over the units that compose it. The Voronoi tesselation is done heuristically following a constant speed
 * homogeneous radial growth process that uses structure from the composition network of the landscape to infer
 * neighborhood between units.
*/
trait VoronoiTesselation extends SpatialStochasticEvents:
  type A <: LandscapeUnit
  val composition: Map[Long,A]
  val structure: Graph[Long,DefaultEdge]

  /**
   * Creates a seeded composition graph to start the tesselation. The function preserves graph structure but changes
   * vertex attributes by the Id of the voronoi polygon which is the id of the seeded unit, if the unit is not seeded
   * then the Id is set to -1L.
   *
   * @param n_seeds is the number of voronoi seeds to effectuate the tesselation
   * @return a seeded graph where vertex attribute is VertexId of seeds in the base landscape.
   */
  def seeded(
              n_seeds: Int,
              rnd: Random
            ):
  Map[Long, Long] = //the id of the base unit, the polygon id
    val seeds: Seq[Long] =
      rnd.shuffle(this.composition.keys).take(n_seeds).toSeq
    this.composition.map {
      case (id,_) => if seeds.contains(id) then (id,id) else (id,-1L)
    }

  /**
   * Calculates the edges of the new composition graph emerging from the tesselation.
   *
   * @param nodes RDD of the vertices Ids and their attribute: a collection of vertices ids from the base composition
   *                 graph.
   * @return an RDD of the edges of the nez landscape. */
  def newEdges(
                nodes: Map[Long, Vector[Long]]
              ):
  List[(Long,Long)] =
    nodes.toSet.subsets(2).collect{
      case s if s.head._2.exists{
        id => neighborListOf(this.structure,id).asScala.toList.exists{
          n => s.last._2.contains(n)
        }
      } => (s.head._1, s.last._1)
    }.toList

  /**
   * Performs a Voronoi tesselation over the composition graph given the number of Voronoi polygons to create.
   * @param n_seeds the number of seeds to perform radial growth from. Seeds are randomly positioned with uniform
   *                probability over the landscape units. This is equivalent to the number of Voronoi polygons.
   * @return a new composition graph where the vertex are collections of the units of the original composition graph's
   *         VertexIds.
   * */
  def tesselate(
                 n_seeds: Int,
                 rnd: Random
               ):
  (Map[Long, Vector[Long]], Graph[Long, DefaultEdge]) =

    @tailrec
    def rec(
             assigned: Map[Long, Long]
           ):
    Map[Long, Long] =
      val remaining: Int = assigned.count(_._2 == -1L)

      if remaining <= 0.0 then assigned
      else{
        val cum_prob = VoronoiTesselation.cumulativeProbabilities(assigned, this.structure)
        val x_rnd: Double = rnd.between(0.0, cum_prob.last._2)
        val pos = selectUnitId( x_rnd, cum_prob )
        val pol = VoronoiTesselation.selectGrowingPolygon( pos, assigned, this.structure, rnd )
        val new_graph = assigned.map( v => if v._1 == pos then (v._1,pol) else v )
        rec(new_graph)
      }

    val seeded = this.seeded(n_seeds,rnd)
    val assigned: Map[Long, Long] = rec(seeded)
    val nodes: Map[Long, Vector[Long]] = VoronoiTesselation.groupByPolygon(assigned)
    val edges: List[(Long,Long)] = this.newEdges(nodes)
    val g: Graph[Long,DefaultEdge] = new SimpleGraph[Long,DefaultEdge](classOf[DefaultEdge])
    edges.foreach{
      (v1,v2) =>
      g.addVertex(v1)
      g.addVertex(v2)
      g.addEdge(v1,v2)
    }
    (nodes,g)

object VoronoiTesselation:

  /**
   * Calculates the expansion probability of a Voronoi polygon.
   * @param assigned the graph over which the radial grozth process is being simulated.
   * @return a VertexRDD containing the expansion probabilities in each vertex of the graph.
   * */
  def cumulativeProbabilities(
                               assigned: Map[Long, Long],
                               structure: Graph[Long, DefaultEdge]
                             ):
  ListMap[Long,Double] =
    ListMap(
      assigned.map{
        v =>
          if v._2 == -1L // If the polygon is not colonized yet
          then {
            val neighbors = neighborListOf(structure,v._1).asScala.toList
            (v._1, neighbors.count(n => assigned.getOrElse(n,-1L) != -1L).toDouble / neighbors.size.toDouble) // Count the number of colonized neighbors
          }
          else (v._1,0.0)
      } .filter(_._2>0.0)
        .toSeq.sortWith(_._2>_._2):_* // sorting from largest probability to smallest to speed up selection in average
    )
      .scanLeft((-1L,0.0))( (pre,now) => (now._1, pre._2 + now._2)).tail.to(ListMap)


  /**
   * Groups VertexIds of the base composition according to the Id of the Voronoi polygon they belong to.
   * @param dispersed the supporting graph after finishing the radial growth process
   * @return an RDD of tuples with the information on the VertexId of the Voronoi polygon and the collection of VertexIds
   *         of base landscape's vertices belonging to the polygon.
   * */
  def groupByPolygon(
                      dispersed: Map[Long, Long]
                    ):
  Map[Long, Vector[Long]] =
    dispersed.toVector.groupBy(_._2).map{
        case (k,v) => (k,v.map(_._1))
    }


  /**
   * Determines the Voronoi polygon that expands at a given time step to a given vertex. Useful in case of competing
   * polygons. The relative probability associated to each competing polygon is proportional to the number of edges
   * between vertices in the polygon and the selected vertex.
   * @param vid the vertex where a polygon will expand to
   * @param assigned the graph supporting the tesselation process.
   * @return the id of the expanding polygon.
   * */
  def selectGrowingPolygon(
                            id: Long,
                            comp: Map[Long, Long],
                            struct: Graph[Long,DefaultEdge],
                            rnd: Random
                          ):
  Long =
      rnd.shuffle(
        neighborListOf(struct,id).asScala.toList.filter(
          n => comp.getOrElse(n,-1L) != -1L
        )
      ) .take(1)
        .head


end VoronoiTesselation
