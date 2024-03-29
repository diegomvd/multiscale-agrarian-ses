package MultiScaleAgrarianSES

/**
Implementation of the Ecological Units. EcoUnits are the elementary constituents of an EcoLandscape and are defined by
their land cover.
@author diego
@todo write the expression of the production in low-intensity EcoUnits.
*/

case class EcoUnit(
                    id: Long,
                    cover: LandCover
                  )
  extends LandscapeUnit:
  /**
   * @param c is the land cover to match against this EcoUnit's cover
   * @return true if there's a match, false if there's not
   * */
  def matchCover(
                  c: LandCover
                ):
  Boolean =
    EcoUnit.matchCover(this.cover,c)

object EcoUnit:

  def apply():
  EcoUnit =
    EcoUnit(-1L, LandCover.NoCover)

  /**
  @param c1 first land cover type
  @param c2 second land cover type
  @return true if both covers are equal, false if not
  */
  def matchCover(
                  c1: LandCover,
                  c2: LandCover
                ):
  Boolean =
    c1 == c2

  /**
  @param s is the sensitivity to ecosystem service inflow
  @param es is the ecosystem service inflow
  @return the recovery/degradation propensity
  */
  def increasingPES(
                     s: Double,
                     es: Double,
                     yes: Double
                   ):
  Double =
    s*scala.math.pow(es,yes)

  def decreasingPES(
                     s: Double,
                     es: Double,
                     yes: Double
                   ):
  Double =
    1-s*scala.math.pow(es,yes)

end EcoUnit

