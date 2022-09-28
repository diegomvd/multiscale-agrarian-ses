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

  /**
   * Calculates the cumulative sum of the spontaneous propensities for each type of event in each unit of the EcoLandscape.
   *
   * @param i_val     the initial value for the cumulative sum of the spontaneous propensities
   * @param es_map the composition graph of the EcoLandscape joined with ecosystem service provision
   * @return a 2-tuple with the maps containing the propensities of each type of event and the last propensity value to continue cumulative sums
   */
  def spontaneousPropensities(
                               i_val: Double,
                               es_map: Map[(Long,EcoUnit), Double]
                             ):
  ((ListMap[(Long,EcoUnit), Double], ListMap[(Long,EcoUnit), Double], ListMap[(Long,EcoUnit), Double], ListMap[(Long,EcoUnit), Double]), Double) =
    val recovery: ListMap[(Long,EcoUnit), Double] = SpontaneousPropensities.propensity(i_val, es_map, this.s_rec, LandCover.Degraded, EcoUnit.increasingPES)
    val degradation: ListMap[(Long,EcoUnit), Double] = SpontaneousPropensities.propensity(recovery.last._2, es_map, this.s_deg, LandCover.Natural, EcoUnit.decreasingPES)
    val lo_floss: ListMap[(Long,EcoUnit), Double] = SpontaneousPropensities.propensity(degradation.last._2, es_map, this.s_flo, LandCover.LowIntensity, EcoUnit.decreasingPES)
    val hi_floss: ListMap[(Long,EcoUnit), Double] = SpontaneousPropensities.propensity(lo_floss.last._2, es_map, this.s_flo, LandCover.HighIntensity, EcoUnit.decreasingPES)
    ((recovery, degradation, lo_floss, hi_floss), hi_floss.last._2)

  /**
   * Selects the type of spontaneous transition given a random number and the spontaneous propensities
   *
   * @param x_rnd the random number thrown to sample the distributions.
   * @param prop  contains the recovery, degradation and fertility loss propensities in field 1,2,3 and 4 respectively
   * @return the event type
   */
  def selectSpontaneous(
                         x_rnd: Double,
                         prop: (ListMap[(Long,EcoUnit), Double], ListMap[(Long,EcoUnit), Double], ListMap[(Long,EcoUnit), Double], ListMap[(Long,EcoUnit), Double])
                       ):
  EventType = 
    x_rnd match 
      case x if x < prop._1.last._2 => EventType.Recovery
      case x if x < prop._2.last._2 => EventType.Degradation
      case x if x < prop._3.last._2 => EventType.LowIntensityFertilityLoss
      case x if x < prop._4.last._2 => EventType.HighIntensityFertilityLoss
    
  
object SpontaneousPropensities:  
  /**
   * Calculates the propensity of each EcoUnit in the EcoLandscape associated with one certain event.
   *
   * @param i_val is the initial value for the cumulative sum
   * @param es    is the ecoServices map
   * @param s     is this transition's sensitivity with es flow
   * @param c     is the land cover type required for this transition
   * @param f     is the function to calculate the propensity of this transition
   * @return a ListMap containing the propensity for a certain transition in each EcoUnit of the EcoLandscape
   */
  def propensity(
                  i_val:Double,
                  es_map:Map[(Long,EcoUnit),Double],
                  s:Double,
                  c:LandCover,
                  f:(Double,Double)=>Double
                ):
  ListMap[(Long,EcoUnit),Double]=
    es_map.filter{
      case ((_,u),_) => u.matchCover(c)
    }.scanLeft( (es_map.head._1, i_val) ){ 
      (pre, now) => ( now._1, f(s,now._2) + pre._2 ) 
    }.to(ListMap)
  
end SpontaneousPropensities
    









