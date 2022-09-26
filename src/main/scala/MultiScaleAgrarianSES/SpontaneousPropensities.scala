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
        es_map:Map[EcoUnit,Double],
        s:Double,
        c:LandCover,
        f:(Double,Double)=>Double):
        ListMap[EcoUnit,Double]=
        es_map.filter((unit,_)=>unit.matchCover(c)).scanLeft((es_map.head._1,i_val))((pre,now)=>(now._1,f(s,now._2)+pre._2)).to(ListMap)

        /**
         * Calculates the propensity of a land recovery transition in each EcoUnit.
         *
         * @param ival     the initial value for the cumulative sum
         * @param es_graph the composition graph of the EcoLandscape joined with ecosystem service provision
         * @param s        the sensitivity of land recovery to ecosystem service provision
         * @return a ListMap with the propensity associated to the VertexId of each EcoUnit in the EcoLandscape
         * */
        def recoveryPropensity(
        i_val:Double,
        es_map:Map[EcoUnit,Double],
        s:Double):
        ListMap[EcoUnit,Double]=
        propensity(i_val,es_map,s,LandCover.Degraded,EcoUnit.increasingPES)

        /**
         * Calculates the propensity of a land degradation transition in each EcoUnit.
         *
         * @param ival     the initial value for the cumulative sum
         * @param es_graph the composition graph of the EcoLandscape joined with ecosystem service provision
         * @param s        the sensitivity of land degradation to ecosystem service provision
         * @return a ListMap with the propensity associated to the VertexId of each EcoUnit in the EcoLandscape
         * */
        def degradationPropensity(
        i_val:Double,
        es_map:Map[EcoUnit,Double],
        s:Double):
        ListMap[EcoUnit,Double]=
        propensity(i_val,es_map,s,LandCover.Natural,EcoUnit.decreasingPES)

        /**
         * Calculates the propensity of a fertility loss transition in each low-intensity EcoUnit.
         *
         * @param ival     the initial value for the cumulative sum
         * @param es_graph the composition graph of the EcoLandscape joined with ecosystem service provision
         * @param s        the sensitivity of fertility loss to ecosystem service provision
         * @return a ListMap with the propensity associated to the VertexId of each EcoUnit in the EcoLandscape
         * */
        def fertilityLossLIPropensity(
        i_val:Double,
        es_map:Map[EcoUnit,Double],
        s:Double):
        ListMap[EcoUnit,Double]=
        propensity(i_val,es_map,s,LandCover.LowIntensity,EcoUnit.decreasingPES)

        /**
         * Calculates the propensity of a fertility loss transition in each high-intensity EcoUnit.
         *
         * @param ival     the initial value for the cumulative sum
         * @param es_graph the composition graph of the EcoLandscape joined with ecosystem service provision
         * @param s        the sensitivity of fertility loss to ecosystem service provision
         * @return a ListMap with the propensity associated to the VertexId of each EcoUnit in the EcoLandscape
         * */
        def fertilityLossHIPropensity(
        i_val:Double,
        es_map:Map[EcoUnit,Double],
        s:Double):
        ListMap[EcoUnit,Double]=
        propensity(i_val,es_map,s,LandCover.HighIntensity,EcoUnit.decreasingPES)

        /**
         * Calculates the cumulative sum of the spontaneous propensities for each type of event in each unit of the EcoLandscape.
         *
         * @param ival     the initial value for the cumulative sum of the spontaneous propensities
         * @param es_graph the composition graph of the EcoLandscape joined with ecosystem service provision
         * @return a 2-tuple with the maps containing the propensities of each type of event and the last propensity value to continue cumulative sums
         */
        def spontaneousPropensities(
        i_val:Double,
        es_map:Map[EcoUnit,Double]):
        ((ListMap[EcoUnit,Double],ListMap[EcoUnit,Double],ListMap[EcoUnit,Double],ListMap[EcoUnit,Double]),Double)={
        val recovery:ListMap[EcoUnit,Double]=recoveryPropensity(i_val,es_map,s_rec)
        val degradation:ListMap[EcoUnit,Double]=degradationPropensity(recovery.last._2,es_map,s_deg)
        val li_floss:ListMap[EcoUnit,Double]=fertilityLossLIPropensity(degradation.last._2,es_map,s_flo)
        val hi_floss:ListMap[EcoUnit,Double]=fertilityLossHIPropensity(li_floss.last._2,es_map,s_flo)
        ((recovery,degradation,li_floss,hi_floss),hi_floss.last._2)
        }
        /**
         * Selects the type of spontaneous transition given a random number and the spontaneous propensities
         *
         * @param x_rnd the random number thrown to sample the distributions.
         * @param prop  contains the recovery, degradation and fertility loss propensities in field 1,2,3 and 4 respectively
         * @return the event type
         */
        def selectSpontaneous(
        x_rnd:Double,
        prop:(ListMap[EcoUnit,Double],ListMap[EcoUnit,Double],ListMap[EcoUnit,Double],ListMap[EcoUnit,Double])):
        EventType={
        x_rnd match{
        case x if x<prop._1.last._2=>EventType.Recovery
        case x if x<prop._2.last._2=>EventType.Degradation
        case x if x<prop._3.last._2=>EventType.LowIntensityFertilityLoss
        case x if x<prop._4.last._2=>EventType.HighIntensityFertilityLoss
        }
        }
