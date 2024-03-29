package MultiScaleAgrarianSES

import scala.math.pow
import scala.math.max


/**
 * Implementation of the Planning Units. A PlnUnit is defined by its composition: a collection of VertexIds of EcoUnits
 * in the EcoLandscape at the base of the PlnLandscape containing this PlnUnit. A PlnUnit is the elementary constituent
 * of a PlnLandscape.
 * @note PlnUnits are set at initialization by performing a Voronoi tesselation over an EcoLandscape and, for the time
 *       being, their composition cannot be modified over the course of a simulation.
 *
 * @author diego
 * @todo determine whether the adjacent function still makes sense.
*/
case class PlnUnit(
                    id: Long,
                    composition: Vector[Long]
                  )
extends LandscapeUnit:
  /**
   * Selects all the PlnUnits adjacent to this PlnUnit.
   * @param r the radius of the EcoLandscape at the base of the PlnLandscape containing this PlnUnit.
   * @return a collection of adjacent PlnUnits.
   * @note Not sure if this feature is of interest
   * */
  //def adjacent(r: Int): ParVector[VertexId] =
  //  PlnUnit.adjacent(r,this.composition)

  /**
   * Determines if this PlnUnit is available for conversion.
   * @param eco the composition of the the EcoLandscape at the base of the PlnLandscape containing this PlnUnit.
   * @return true if available, false if not.
   * */
  def isAvailable(
                   eco: Map[Long,EcoUnit]
                 ): 
  Boolean =
    PlnUnit.isAvailable(this.composition, eco)

object PlnUnit :

  def apply():
  PlnUnit =
    PlnUnit(-1L, Vector())

  /**
  //@param r the radius of the EcoLandscape at the base of the PlnLandscape containing this PlnUnit.
  //@param comp the composition of the PlnUnit
  //@return an RDD with the IDs of each ecological unit adjacent to the planning unit
  */
  /**
  // * TODO: Not sure this function is needed, if it is need to correct.
  def adjacent(
    r: Int,
    comp: ParVector[VertexId]):
  ParVector[VertexId] =
    comp.map( vid => ModCo.neighbors(vid.toInt,r,1) ).filterNot( n => comp.exists(_ == n.toLong ))
  */
  /**
  @param comp the composition of the PlnUnit
  @param eco the composition of the the EcoLandscape at the base of the PlnLandscape containing this PlnUnit.
  @return true if the planning unit can be cultivated, false if not
  */
  def isAvailable(
                   comp: Vector[Long],
                   eco: Map[Long,EcoUnit]
                 ):
  Boolean =
    // get all the eco units inside this planning unit
    val plnU: Vector[EcoUnit] = eco.collect {
      case (id, u) if comp.contains(id) => u
    }.toVector
    // check if there is at least one natural unit that can be converted
    val predicate1 = plnU.exists{
      _.matchCover(LandCover.Natural)
    }
    // now check that there are not agricultural units
    val predicate2 = plnU.exists(
      u => u.matchCover(LandCover.LowIntensity) || u.matchCover(LandCover.HighIntensity)
    )
    predicate1 && !predicate2

  /**
  @param nn is the number of neighbors that give weight to the clustering
  @param clustering is the clustering coefficient
  @return the clustering weight
  */
  def weightExpression(
                        nn: Int,
                        clustering: Double
                      ):
  Double =
    pow( max(0.1,nn), clustering)

end PlnUnit
