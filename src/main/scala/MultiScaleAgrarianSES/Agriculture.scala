package MultiScaleAgrarianSES

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
 * Implementation of Agriculture: a trait used to extend a Landscape.
 * @todo check mapValues and reduce
 */
trait Agriculture:
  /**
  yes is the contribution of ES to resource production in LI units
  his is the number of households that can be supported by the production of one HI unit
  */
  val yes: Double
  val his: Double
  def resourceProduction(
                          es_graph: Graph[(EcoUnit,Double), UnDiEdge]
                        ):
  Double =
    Agriculture.calculateProduction(es_graph,yes,his)

object Agriculture:
  /**
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param yes is the contribution of ecosystem services to low-intensity agricultural production
  @param his is the number of households that are maintained with the production of one high-intensity unit
  @return the total amount of resources produced in the low-intensity units
  */
  def lowIntResources(
                       es_graph: Graph[(EcoUnit,Double), UnDiEdge],
                       yes: Double,
                       his: Double
                     ):
  Double =
    // traverses the set of nodes and sums contribution when the EcoUnit is of LowIntensity type
    es_graph.nodes.foldLeft[Double](0.0){
      (production, n) =>
        if n.toOuter._1.matchCover(LandCover.LowIntensity)
        then production + EcoUnit.lowIntResEquation(yes,his,n.toOuter._2)
        else production
    }

  /**
  @return the total amount of resources produced in the high-intensity units
  */
  def highIntResources(
                        es_graph: Graph[(EcoUnit,Double), UnDiEdge]
                      ):
  Double =
    es_graph.nodes.foldLeft[Double](0.0) {
      (production, n) =>
        if n.toOuter._1.matchCover(LandCover.HighIntensity)
        then production + EcoUnit.highIntResEquation()
        else production
    }

  /**
  @return the total amount of resources produced in the landscape
  */
  def calculateProduction(
                           es_graph: Graph[(EcoUnit,Double), UnDiEdge],
                           yes: Double,
                           his: Double
                         ):
  Double =
      lowIntResources(es_graph,yes,his) + highIntResources(es_graph)

end Agriculture
