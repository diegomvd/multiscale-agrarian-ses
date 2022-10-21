package MultiScaleAgrarianSES
/**
 * A top landscape is a landscape built on top of a base landscape given its relative scale to the base landscape. A top
 * landscape can also be a base landscape, as it is the case currently with the planning landscape. This feature allows
 * for the construction of nested landscapes' hierarchies.
 * */
trait TopLandscape extends Landscape :
  val unitRadius: Int

object TopLandscape :
  def numberOfUnits(
                     unitRadius: Int,
                     baseSize: Int
                   ):
  Int = 
    (baseSize.toDouble / unitArea(unitRadius).toDouble).toInt
  def numberOfUnits(
                     unitRadius:Int,
                     unitRadiusBase: Int,
                     baseSize: Int
                   ):
  Int = 
    (baseSize*unitArea(unitRadiusBase).toDouble/unitArea(unitRadius*unitRadiusBase).toDouble).toInt
  def unitArea(
                unitRadius: Int
              ):
  Int =
    ModCo.area(unitRadius)
  
end TopLandscape

