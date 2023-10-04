package MultiScaleAgrarianSES

import scala.collection.immutable.ListMap

/**
 * Implementation of the propensities of spontaneous events in an EcoLandscape as a trait. An EcoLandscape extended by
 * SpontaneousPropensities supports the calculation of the propensities used to select stochastic events.
 */
trait SpontaneousPropensities:

  val s_rec:Double
  val s_deg:Double
  val s_flo:Double
  val yes: Double
  val composition: Map[Long,EcoUnit]

  /**
   * Calculates the cumulative sum of the spontaneous propensities for each type of event in each unit of the EcoLandscape.
   *
   * @param i_val     the initial value for the cumulative sum of the spontaneous propensities
   * @param es_map the composition graph of the EcoLandscape joined with ecosystem service provision
   * @return a 2-tuple with the maps containing the propensities of each type of event and the last propensity value to continue cumulative sums
   */
  def spontaneousPropensities(
                               i_val: Double,
                               es_map: Map[Long,Double]
                             ):
  ((ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double]), Double) =

    val (recovery,rl): (ListMap[Long, Double],Double) = SpontaneousPropensities.propensity(i_val, this.composition, es_map, this.s_rec, this.yes, LandCover.Degraded, EcoUnit.increasingPES)
    val (degradation,dl): (ListMap[Long, Double],Double) = SpontaneousPropensities.propensity(rl, this.composition, es_map, this.s_deg, this.yes, LandCover.Natural, EcoUnit.decreasingPES)
    val (lo_floss,rlo): (ListMap[Long, Double],Double) = SpontaneousPropensities.propensity(dl, this.composition, es_map, this.s_flo, this.yes, LandCover.LowIntensity, EcoUnit.decreasingPES)
    val (hi_floss,rhi): (ListMap[Long, Double],Double) = SpontaneousPropensities.propensity(rlo, this.composition, es_map, this.s_flo, this.yes, LandCover.HighIntensity, EcoUnit.decreasingPES)
    val total: Double = List(rl, dl, rlo, rhi).max
    ((recovery, degradation, lo_floss, hi_floss), total)

  /**
   * Selects the type of spontaneous transition given a random number and the spontaneous propensities
   *
   * @param x_rnd the random number thrown to sample the distributions.
   * @param prop  contains the recovery, degradation and fertility loss propensities in field 1,2,3 and 4 respectively
   * @return the event type
   */
  def selectSpontaneous(
                         x_rnd: Double,
                         prop: (ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double])
                       ):
  EventType = 
    x_rnd match 
      case x if prop._1.nonEmpty && x < prop._1.last._2 => EventType.Recovery
      case x if prop._2.nonEmpty && x < prop._2.last._2 => EventType.Degradation
      case x if prop._3.nonEmpty && x < prop._3.last._2 => EventType.LowIntensityFertilityLoss
      case x if prop._4.nonEmpty && x < prop._4.last._2 => EventType.HighIntensityFertilityLoss
    
  
object SpontaneousPropensities:  
  /**
   * Calculates the propensity of each EcoUnit in the EcoLandscape associated with one certain event.
   *
   * @param i_val is the initial value for the cumulative sum
   * @param es_map    is the ecoServices map
   * @param s     is this transition's sensitivity with es flow
   * @param c     is the land cover type required for this transition
   * @param f     is the function to calculate the propensity of this transition
   * @return a ListMap containing the propensity for a certain transition in each EcoUnit of the EcoLandscape
   */
  def propensity(
                  i_val:Double,
                  comp: Map[Long,EcoUnit],
                  es_map:Map[Long,Double],
                  s:Double,
                  yes: Double,
                  c:LandCover,
                  f:(Double,Double,Double)=>Double
                ):
  (ListMap[Long,Double], Double)=

    val p = es_map.filter{
      case (id,_) => comp.getOrElse(id,EcoUnit()).matchCover(c)
    }.scanLeft( (-1L, i_val) ){
      (pre, now) => ( now._1, f(s,now._2,yes) + pre._2 )
    }.tail.to(ListMap)
    if p.isEmpty then (p,i_val) else (p,p.last._2)

  def updateInitialValue(
                          propensities: ((ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double]), Double),
                          diff: Double
                        ):
  ((ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double]), Double) =
    val recovery = propensities._1._1.map( (id,x) => (id,x+diff) )
    val degradation = propensities._1._2.map( (id,x) => (id,x+diff) )
    val lo_floss = propensities._1._3.map( (id,x) => (id,x+diff) )
    val hi_floss = propensities._1._4.map( (id,x) => (id,x+diff) )
    val total: Double = List(recovery.lastOption.getOrElse((0L,0.0))._2, degradation.lastOption.getOrElse((0L,0.0))._2, lo_floss.lastOption.getOrElse((0L,0.0))._2, hi_floss.lastOption.getOrElse((0L,0.0))._2).max
    ((recovery,degradation,lo_floss,hi_floss),total)

end SpontaneousPropensities
    









