package MultiScaleAgrarianSES
/**
 * A top landscape is a landscape built on top of a base landscape given its relative scale to the base landscape. A top
 * landscape can also be a base landscape, as it is the case currently with the planning landscape. This feature allows
 * for the construction of nested landscapes' hierarchies.
 * */
trait TopLandscape extends Landscape :
  val unitArea: Int

object TopLandscape :
  def numberOfUnits(
                     unitArea: Int,
                     referenceArea: Int
                   ):
  Int = 
    (referenceArea.toDouble / unitArea.toDouble).toInt

end TopLandscape

