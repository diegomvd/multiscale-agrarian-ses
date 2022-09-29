package MultiScaleAgrarianSES

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.math.pow
import scala.util.Random as rnd
import scala.collection.parallel.immutable.ParVector

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

/**
 *
 * Implementation of the landscape at a biophysical level. An EcoLandscape is composed by EcoUnits stored in a graph where
 * edges represent functional connectivity between units rather that adjacency in space. The class parameters are:
 * ecr: Ecological Connectivity Range
 * scal_exp: the scaling exponent of the power-law ecosystem services-area relationship
 * yes: contribution of ecosystem services to agricultural production in low-intensity units
 * his: the number of households that can be supported by a single high-intensity unit. This is used in the production
 * function.
 * s_rec: sensitivity of land recovery propensity to ecosystem service provision. Higher sensitivity means more response
 * to ecosystem services.
 * s_deg: idem for degradation.
 * s_flo: idem for fertility loss.
 *
 * The EcoLandscape is first instantiated to initialize the PlnLandscape, MngLandscape and HumanPop, and then initialized
 * using these three objects to produce an initial state as close to equilibrium with the population size as possible and
 * with a spatial structure that mimics the spatial processes of the simulation, given the parameters. This effort in
 * coherence, instead of just a random landscape, is to guarantee that the transient at the beginning of the simulation
 * is as short as possible and that the simulation does not start with a regime change.
 * @todo where should the spark context be declared and setted, if at some top level class then I might need to pass it
 *       as argument to a bunch of functions
*/
case class EcoLandscape(
                         composition: Map[Long,EcoUnit],
                         structure: Graph[Long,UnDiEdge], 
                         size: Int,
                         ecr: Int,
                         scal_exp: Double,
                         yes: Double,
                         his: Double,
                         s_rec: Double,
                         s_deg: Double,
                         s_flo: Double)
  extends BaseLandscape with Agriculture with EcoServices with SpontaneousPropensities with SpatialStochasticEvents :

    /**
     * Updates land cover in multiple units following a conversion event.
     * @param old_units is a collection of VertexIDs to be updated in cover
     * @param new_units is the new cover
     * @return an updated EcoLandscape
     **/
    def update(
                old_units: Vector[Long],
                new_units: Vector[EcoUnit]
              ):
    EcoLandscape =
      val comp = this.updateComposition(old_units,new_units)
      this.copy(composition = comp)

    /**
     * Updates land cover in a single unit following a spontaneous event.
     * @param vid  is the VertexID to be updated in cover
     * @param cover is the new cover
     * @return an updated EcoLandscape
     * */
    def update(
                old_unit: Long,
                new_unit: EcoUnit
              ):
    EcoLandscape =
      val comp = this.updateComposition(old_unit,new_unit)
      this.copy(composition = comp)

    /**
     * Determines in which units a conversion event happens and what is the resulting cover from the transition:
     * LowIntensity or HighIntensity. The procedure starts by spatializing the conversion propensity over the management
     * landscape to choose a management unit and repeating this procedure over the planning landscape until reaching the
     * ecological level. Type of resulting land cover is determined by the strategy in the management unit where the
     * transition happens.
     * @param x_rnd the random number to sample the probability distribution.
     * @param i_val initial value to calculate the propensity in each MngUnit.
     * @param pln the planning landscape.
     * @param mng the management landscape.
     * @param tcp the total conversion propensity.
     * @return a tuple with the unit's VertexIds and the resulting cover.
     * */
    def resolveConversionEvent(
                                x_rnd: Double,
                                i_val: Double,
                                pln: PlnLandscape,
                                mng: MngLandscape,
                                tcp: Double
                              ):
    (Vector[Long], Vector[EcoUnit]) =
      // First calculate the conversion propensity in each management unit
      val mngP: ListMap[Long, Double] = mng.propensityOfMngUnits(i_val, tcp, pln.composition, this.composition)
      // Get the Id of the selected unit, and the upper bound for the unit's propensity
      val (mngId,upperP): (Long,Double) = mng.selectUnitIdWithPropensity(x_rnd,mngP)
      // Get the lower bound of the unit's propensity: this is not efficient because of ListMap
      val lowerP: Double = mngP.getOrElse(mngId-1L,mngP.head._2)
      // Get the management unit at the selected Id
      val mngU: MngUnit = mng.composition.getOrElse(mngId,MngUnit())
      // Calculate the propensities of planning units within the selected management unit
      val plnP: ListMap[Long, Double] =
        mngU.propensityOfPlnUnits(lowerP, upperP-lowerP, pln, this.composition)
      // Select a planning unit for conversion
      val plnId: Long  = pln.selectUnitId(x_rnd, plnP)
      // Finally, get the Ids of the ecological units to convert to agriculture from the planning unit composition
      val ecoIds: Vector[Long] = pln.composition.getOrElse(plnId, PlnUnit()).composition
      mngU.strategy match
        case MngStrategy.LandSharing => (ecoIds, ecoIds.map( EcoUnit(_, LandCover.LowIntensity) ) )
        case MngStrategy.LandSparing => (ecoIds, ecoIds.map( EcoUnit(_, LandCover.HighIntensity) ) )


    def initialize(
                    pln: PlnLandscape,
                    mng: MngLandscape,
                    f_agr: Double,
                    f_deg: Double
                  ):
    EcoLandscape =
      EcoLandscape.initialize(this,pln,mng,f_agr,f_deg)

    def countNatural: Int = this.composition.count{ case (_,u) => u.matchCover(LandCover.Natural) }
    def countDegraded: Int = this.composition.count{ case (_,u) => u.matchCover(LandCover.Degraded) }
    def countAgriculturalLo: Int = this.composition.count{ case (_,u) => u.matchCover(LandCover.LowIntensity) }
    def countAgriculturalHi: Int = this.composition.count{ case (_,u) => u.matchCover(LandCover.HighIntensity) }
    def countAgricultural: Int = this.countAgriculturalLo + this.countAgriculturalHi

object EcoLandscape :

  /**
  *This method overloads the pre-given apply method and is to be used when initializing the system. The function builds
  *a fully natural EcoLandscape from the EcoLandscape parameter values. Initialization with simulation's initial values
  *is done in initialize function defined in the companion case class.
  * @constructor
  * @param r is the landscape's radius
  * @param ecr is the ecological connectivity range and determines biophysical connections between units
  * @param scal_exp is the scaling exponent of the power-law ecosystem services - area relationship
  * @param yes is the contribution of ecosystem services to production in low-intensity units
  * @param his is the number of households that are sustained by one high-intensity unit
  * @param s_rec is land recovery sensitivity to ecosystem service provision
  * @param s_deg is land degradation sensitivity to ecosystem service provision
  * @param s_flo is fertility loss sensitivity to ecosystem service provision
  * @return an EcoLandscape
  */

  def apply(
             r: Int,
             ecr: Int,
             scal_exp: Double,
             yes: Double,
             his: Double,
             s_rec: Double,
             s_deg: Double,
             s_flo: Double
           ):
  EcoLandscape =
    val comp = buildComposition(r)
    val struct = buildStructure(comp,ecr)
    EcoLandscape(comp,struct,ModCo.area(r),ecr,scal_exp,yes,his,s_rec,s_deg,s_flo)

  /**
  @param r is the radius of the biophysical landscape
  @param ecr is the ecological connectivity range
  @return the biophysical composition graph with every unit in a natural state
  */
  def buildComposition(
                        r: Int
                      ):
  Map[Long,EcoUnit] =
    ModCo.apply(r).map { m => (m.toLong,EcoUnit(m.toLong,LandCover.Natural)) }.toMap

  def buildStructure(
                      composition: Map[Long,EcoUnit],
                      ecr: Int
                    ):
  Graph[Long, UnDiEdge] =
    val nodes: List[Long] = composition.keys.toList
    val edges = // List[UnDiEdge]
      nodes.toSet.subsets(2).collect {
        case s if ModCo.neighbors(s.head.toInt, r, ecr).contains(s.last) =>
          UnDiEdge(s.head, s.last)
      }.toList
    Graph.from(nodes,edges)


  /**
  @param fagr is fraction of agricultural units in the initial biophysical landscape
  @param fdeg is fraction of degraded units in the initial biophysical landscape
  @return a biophysical landscape initialized according to the simulation parameter values
  */
  def initialize(
                  eco: EcoLandscape,
                  pln: PlnLandscape,
                  mng: MngLandscape,
                  f_agr: Double,
                  f_deg: Double
                ):
  EcoLandscape =
    /**
    TODO: check this
    */
    def initializeAgriculturalUnit(
                                    eco: EcoLandscape,
                                    pln: PlnLandscape,
                                    mng: MngLandscape
                                  ):
    EcoLandscape =
      val x_rnd: Double = rnd.nextDouble( )
      val res = eco.resolveConversionEvent(x_rnd,0.0,pln,mng,1.0)
      eco.update(res._1,res._2)

    // TODO: it remains to be decided if using selection, and thus propensity calculation over ids or units themselves
    //  the spontaneous propensities page needs to be updated if this is ever done on the units and the functions should
    //  take LandscapeUnits as arguments if this ever works
    def initializeDegradedUnit(
                                eco: EcoLandscape
                              ):
    EcoLandscape =
      val propensity: ListMap[Long,Double] =  SpontaneousPropensities.propensity(0.0, eco.composition, eco.ecoServices, 1.0, LandCover.Natural, EcoUnit.decreasingPES)
      val x_rnd: Double = rnd.between(0.0,propensity.last._2)
      val ecoId = eco.selectUnitId(x_rnd,propensity)
      eco.update(ecoId, EcoUnit(ecoId, LandCover.Degraded))

    /**
    @param n is a tuple with the number of remaining agricultural units to put first and the remaining degraded units second
    @param transition is the type of transition that was simulated
    @param step is the number of units that have transitioned
    @return a new tuple with the updated numbers
    */
    def updateRemaining(
                         n: (Int,Int),
                         transition: EventType,
                         step: Int
                       ):
    (Int,Int) =
      transition match
        case EventType.Conversion => val upd_n_deg = n._2
          if n._1>0 then {
            val upd_n_agr = n._1 - step
            (upd_n_agr,upd_n_deg)
          }
          else {
            val upd_n_agr = n._1
            (upd_n_agr,upd_n_deg)
          }
        case EventType.Degradation => val upd_n_agr = n._1
          if n._2>0 then {
            val upd_n_deg = n._2 - step
            (upd_n_agr,upd_n_deg)
          }
          else {
            val upd_n_deg = n._1
            (upd_n_agr, upd_n_deg)
          }
      

    @tailrec
    def rec(
             eco: EcoLandscape,
             pln: PlnLandscape,
             mng: MngLandscape,
             n_agr: Int,
             n_deg: Int
           ):
    EcoLandscape =
      val n: Int = n_agr + n_deg
      if n==0 then eco
      else {
        rnd.nextInt(n) match 
          case n_rnd if n_rnd<n_agr =>  // Conversion transition is chosen
            val old_agr: Int = eco.countAgricultural
            val upd_eco: EcoLandscape = initializeAgriculturalUnit(eco, pln, mng)
            val new_agr: Int = upd_eco.countAgricultural
            val step: Int = new_agr - old_agr
            val n_remaining: (Int,Int) = updateRemaining((n_agr,n_deg),EventType.Conversion,step)
            rec(upd_eco, pln, mng, n_remaining._1, n_remaining._2)

          case n_rnd if n_rnd<n_deg =>  // Degradation transition is chosen
            val upd_eco = initializeDegradedUnit(eco)
            val n_remaining: (Int,Int) = updateRemaining((n_agr,n_deg),EventType.Degradation,1)
            rec(upd_eco, pln, mng, n_remaining._1, n_remaining._2)
      }
      

    val n_agr: Int = eco.size * f_agr.toInt
    val n_deg: Int = eco.size * f_deg.toInt
    rec(eco, pln, mng, n_agr, n_deg)
          
end EcoLandscape
