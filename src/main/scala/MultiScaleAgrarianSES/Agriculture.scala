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
  val composition: Map[Long,EcoUnit]

  def resourceProduction(
                          es: Map[Long,Double]
                        ):
  Double =
    Agriculture.calculateProduction(this.composition,es,yes,his)

object Agriculture:
  /**
  @param es_graph is the biophysical composition of the landscape joined with the es flow
  @param yes is the contribution of ecosystem services to low-intensity agricultural production
  @param his is the number of households that are maintained with the production of one high-intensity unit
  @return the total amount of resources produced in the low-intensity units
  */
  def lowIntResources(
                       comp: Map[Long,EcoUnit],
                       es: Map[Long,Double],
                       yes: Double,
                       his: Double
                     ):
  Double =
    // traverses the set of nodes and sums contribution when the EcoUnit is of LowIntensity type
    es.foldLeft[Double](0.0){
      (production, n) =>
        if comp.getOrElse(n._1,EcoUnit()).matchCover(LandCover.LowIntensity)
        then production + EcoUnit.lowIntResEquation(yes,his,n._2)
        else production
    }

  /**
  @return the total amount of resources produced in the high-intensity units
  */
  def highIntResources(
                        comp: Map[Long, EcoUnit],
                        es: Map[Long, Double] // note ecosystem ser ices are not really needed for HI in this version of the model
                      ):
  Double =
    es.foldLeft[Double](0.0) {
      (production, n) =>
        if comp.getOrElse(n._1,EcoUnit()).matchCover(LandCover.HighIntensity)
        then production + EcoUnit.highIntResEquation()
        else production
    }

  /**
  @return the total amount of resources produced in the landscape
  */
  def calculateProduction(
                           comp: Map[Long, EcoUnit],
                           es: Map[Long, Double],
                           yes: Double,
                           his: Double
                         ):
  Double =
      lowIntResources(comp,es,yes,his) + highIntResources(comp,es)

end Agriculture
