package MultiScaleAgrarianSES

/**
 * Implementation of Agriculture: a trait used to extend a Landscape.
 * @todo check mapValues and reduce
 */
trait Agriculture:
  /**
  yes is the contribution of ES to resource production in LI units
  */
  val yes: Double
  val composition: Map[Long,EcoUnit]

  def resourceProduction(
                          es: Map[Long,Double]
                        ):
  Double =
    Agriculture.calculateProduction(this.composition,es,yes)

object Agriculture:
  /**
  @param es is the biophysical composition of the landscape joined with the es flow
  @param yes is the contribution of ecosystem services to low-intensity agricultural production
  @return the total amount of resources produced in the low-intensity units
  */
  def lowIntResources(
                       comp: Map[Long,EcoUnit],
                       es: Map[Long,Double],
                       yes: Double
                     ):
  Double =
    // traverses the set of nodes and sums contribution when the EcoUnit is of LowIntensity type
    es.foldLeft[Double](0.0){
      (production, n) =>
        if comp.getOrElse(n._1,EcoUnit()).matchCover(LandCover.LowIntensity)
        then production + lowIntResEquation(yes,n._2)
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
        then production + highIntResEquation()
        else production
    }

  /**
  @return the total amount of resources produced in the landscape
  */
  def calculateProduction(
                           comp: Map[Long, EcoUnit],
                           es: Map[Long, Double],
                           yes: Double
                         ):
  Double =
      lowIntResources(comp,es,yes) + highIntResources(comp,es)

  /**
   *  @param yes is
   *  @param es is the ecosystem service inflow
   *  @return the resource production in a low intensity unit
   */
  def lowIntResEquation(
                         yes: Double,
                         es: Double
                       ):
  Double =
    (1.0 - yes) + yes * es


  def highIntResEquation():
  Double =
    1.0

end Agriculture
