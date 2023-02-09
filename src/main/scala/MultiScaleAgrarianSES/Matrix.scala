package MultiScaleAgrarianSES

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.math.log
import scala.util.Random

/**
 * Implementation of the complete social-ecological system. The Matrix is the digital representation of the system and
 * is defined by its internal time and the sub-components of the system: the ecological, planning and management
 * landscapes and the human population depending on and managing the landscape.
 * @author diego
*/
case class Matrix(
                   t: Double,
                   eco: EcoLandscape,
                   pln: PlnLandscape,
                   mng: MngLandscape,
                   pop: HumanPop,
                   rnd: Random
                 ):

  /**
   * Determines whether a simulation is to be terminated. A simulation is terminated if the Matrix's internal time
   * exceeds the maximum simulation time or if the system reaches ana absorbant state: null population size or fully
   * degraded landscape (even if population is not null it will eventually die-off).
   * @param maxT the maximum simulation time.
   * @return True if the simulation is to be terminated, False if not.
  */
  def doesNotHaveNext(
                       maxT: Double
                     ):
  Boolean =
    val pred_time: Boolean = this.t > maxT
    val pred_pop: Boolean = this.pop.size == 0
    val pred_deg: Boolean = this.eco.countNatural == 0
    pred_time || pred_pop || pred_deg

  /**
   * Runs a simulation of the Matrix's dynamics. Each simulation step consists on:
   * 1- Calculate the ecosystem services flow in the ecological landscape,
   * 2- Calculate resource production,
   * 3- Calculate the propensities of each event,
   * 4- Randomly choose a next event time and next event type,
   * 5- Update the Matrix's state: time and change in landscape or population.
   * @param maxT the maximum simulation time.
   * @return the state of the Matrix at the end of the simulation.
  */
  def simulate(
                maxT: Double
              ):
  Matrix =

    /**
     * Tail recursive function describing each simulation step.
     * @param world the Matrix being simulated.
     * @param maxT the maximum simulation time.
     * @param ncc the ecological landscape's natural connected components.
     * @param es the joined ecosystem services flow graph.
     * @param res the amount of resources available for the population.
     * @param popp the propensities of birth and death.
     * @param spontp the propensities of spontaneous landscape events.
     * @param tcp the total conversion propensity.
     * @return the state of the Matrix at the end of the simulation step.
     * */
    @tailrec
    def rec(
             world: Matrix,
             maxT: Double,
             es: Map[Long,Double],
             res: Double,
             popP: (Double,Double),
             spontP: ((ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double]), Double),
             tcP: Double,
           ):
    Matrix =
      // return the current state of the Matrix if the simulation is to stop
      if world.doesNotHaveNext(maxT) then world
      else {
        // get the new world and in function of the event type actualize
        // propensities and/or ecosystem services or not
        val (new_world, event): (Matrix, EventType) = Matrix.update(popP,spontP,tcP,world,this.rnd)
        // match the event type
        event match {
          case EventType.Demographic =>  // only the population and conversion propensities are updated
            val new_popP = new_world.pop.demographicPropensities(0.0,res)
            val new_tcP = new_world.pop.totalConversionPropensity(res)
            rec(new_world,maxT,es,res,new_popP,spontP,new_tcP)

          case EventType.HighIntensityFertilityLoss => // ecosystem service provision is unchanged by this event, substantial time gain
            val new_res = res - 1.0 // this is just loosing one high intensity unit
            val new_popP = new_world.pop.demographicPropensities(0.0,new_res)
            val new_spontP = new_world.eco.spontaneousPropensities(new_popP._2,es)
            val new_tcP = new_world.pop.totalConversionPropensity(new_res)
            rec(new_world,maxT,es,new_res,new_popP,new_spontP,new_tcP)

          case _ =>  // for any other event update everything
            val new_es: Map[Long,Double] = new_world.eco.ecoServices
            val new_res = new_world.eco.resourceProduction(new_es)
            val new_popP = new_world.pop.demographicPropensities(0.0,new_res)
            val new_spontP = new_world.eco.spontaneousPropensities(popP._2,new_es)
            val new_tcP = new_world.pop.totalConversionPropensity(new_res)
            rec(new_world,maxT,new_es,new_res,new_popP,new_spontP,new_tcP)
        }
      }

    val es = this.eco.ecoServices
    val res = this.eco.resourceProduction(es)
    val popP = this.pop.demographicPropensities(0.0, res)
    val spontP = this.eco.spontaneousPropensities(popP._2, es)
    val tcP = this.pop.totalConversionPropensity(res)

    rec(this, maxT, es, res, popP, spontP, tcP)

  def outputStaticLandscapeOptimization(
                                         n: Int
                                       ):
  (Double,Double,Int) =
    println("Calculating ES average and robustness...")
    val (avg,rob) = this.eco.averageAndRobustnessEcoServices(n,this.rnd)
    println("Results:")
    (avg,rob,this.pop.size)

object Matrix :

  /**
   * Matrix constructor at time 0.0.
   * @param eco the ecological landscape.
   * @param pln the planning landscape.
   * @param mng the management landscape.
   * @param pop the human population.
   * @return an initialized Matrix ready to be simulated.
   * @note the subcomponents of the matrix are initialized individually inside a ScalaTask within OpenMole and then
   *       this constructor is called in the same ScalaTask.
  */
  def apply(
             eco: EcoLandscape,
             pln: PlnLandscape,
             mng: MngLandscape,
             pop: HumanPop,
             rnd: Random
           ):
  Matrix =
    Matrix(0.0,eco,pln,mng,pop,rnd)

  /**
   * Updates the Matrix's state by choosing next event time and type from the event propensities.
   * @param pop    is the human population propensity
   * @param spont  are the spontaneous propensities
   * @param tcp    is the total conversion propensity
   * @return a tuple with the updated world and the event type
  */
  def update(
              popP: (Double,Double),
              spontP: ((ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double], ListMap[Long, Double]), Double),
              tcP: Double,
              world: Matrix,
              rnd: Random
            ):
  (Matrix, EventType) =
    // expression for the next event time
    val new_t: Double = world.t - 1.0/log(rnd.between(0.0, popP._2 + spontP._2 + tcP))
    // random number to select an event, maximum is the sum of the cumulative propensities
    val x_rnd: Double = rnd.between(0.0, popP._2 + spontP._2 + tcP)

    selectEventType(x_rnd,popP._2,spontP._2,tcP) match {
      case EventType.Demographic =>
        val upd_pop: HumanPop = world.pop.update(HumanPop.selectBirthOrDeath(x_rnd,popP))
        (world.copy(t = new_t, pop = upd_pop), EventType.Demographic)
      case EventType.Spontaneous =>
        world.eco.selectSpontaneous(x_rnd,spontP._1) match {
          case EventType.Recovery =>
            val unitId = world.eco.selectUnitId(x_rnd, spontP._1._1)
            val upd_eco: EcoLandscape = world.eco.update(unitId, EcoUnit(unitId,LandCover.Natural))
            (world.copy(t = new_t, eco = upd_eco), EventType.Recovery)
          case EventType.Degradation =>
            val unitId = world.eco.selectUnitId(x_rnd, spontP._1._2)
            val upd_eco: EcoLandscape = world.eco.update( unitId, EcoUnit(unitId,LandCover.Degraded))
            (world.copy(t = new_t, eco = upd_eco), EventType.Degradation)
          case EventType.LowIntensityFertilityLoss =>
            val unitId = world.eco.selectUnitId(x_rnd, spontP._1._3)
            val upd_eco: EcoLandscape = world.eco.update( unitId, EcoUnit(unitId,LandCover.Natural))
            (world.copy(t = new_t, eco = upd_eco), EventType.LowIntensityFertilityLoss)
          case EventType.HighIntensityFertilityLoss =>
            val unitId = world.eco.selectUnitId(x_rnd, spontP._1._4)
            val upd_eco: EcoLandscape = world.eco.update( unitId, EcoUnit(unitId,LandCover.Degraded))
            (world.copy(t = new_t, eco = upd_eco), EventType.HighIntensityFertilityLoss)
          case _ =>
            println("Wrong type of event in Matrix:update: should be a spontaneous one")
            (world, EventType.NoEvent)
        }
      case EventType.Conversion =>
        val (ids, units): (Vector[Long], Vector[EcoUnit]) = world.eco.resolveConversionEvent(x_rnd,spontP._2,world.pln,world.mng,tcP)
        val upd_eco: EcoLandscape = world.eco.update(ids, units)
        (world.copy(t = new_t, eco = upd_eco), EventType.Conversion)
      case _ =>
        println("Wrong type of event in Matrix:update")
        (world, EventType.NoEvent)
    }

  /**
   * Determines the type of the next event's given the propensities.
   * @param x_rnd is the random number thrown to sample the distributions.
   * @param pop  is the total population propensity.
   * @param spont is the total spontaneous + population propensities.
   * @param tcp  is the total management + spontaneous + population propensities.
   * @return the next event's type.
  */
  def selectEventType(
                       x_rnd: Double,
                       popP: Double,
                       spontP: Double,
                       tcP: Double
                     ):
  EventType =
    x_rnd match
      case x if x < popP => EventType.Demographic
      case x if x < spontP => EventType.Spontaneous
      case x if x < tcP => EventType.Conversion


end Matrix
