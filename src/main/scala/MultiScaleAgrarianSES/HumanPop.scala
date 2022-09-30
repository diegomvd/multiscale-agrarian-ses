package MultiScaleAgrarianSES

/**
Implementation of the Human Population. HumanPop is defined by its size and its sensitivity to resource demand which 
determines the celerity of human agricultural expansion response to a lack of resources.
@note the total conversion propensity is calculated from the HumanPop case class.
@author diego
TODO: Still need to write the function for the death propensity
*/

case class HumanPop(
                     size: Int,
                     s_res: Double,
                     his: Double
                   ):

  /**
  @param demo is the type of demographic event: a birth or death
  @return a human population with an updated size and identical sensitivity to
          resource deficit
  */
  def update(
              demo: EventType
            ):
  HumanPop =
    demo match {
      case EventType.Birth => this.copy(size = HumanPop.birth(this.size))
      case EventType.Death => this.copy(size = HumanPop.death(this.size))
      case _ => println("Wrong type of event in HumanPop:update"); this
    }

  /**
  @return the resource demand of the human population given its size and available
          resource
  */
  def resourceDemand(
                      resources: Double
                    ):
  Double =
    this.size.toDouble - resources

  /**
  @return the total conversion propensity given the resource demand and the
          sensitivity to resource demand
  */
  def totalConversionPropensity(
                                 resources: Double
                               ):
  Double =
    this.s_res * this.resourceDemand(resources)

  /**
  @param ival is the initial value for the cummulative sum of the propensities
  @return a tuple with birth and death propensities in first and second positions respectively
  */
  def demographicPropensities(
                               i_val: Double,
                               resources: Double
                             ):
  (Double,Double) =
    val birth: Double = HumanPop.birthPropensity(i_val)
    if resources > 0.0 then
      val death: Double = HumanPop.deathPropensity(birth,this.size,resources,this.his)
      (birth,death)
    else // If resources is 0.0 then set it to his/1000.0 to avoid division by 0.0. This will be small enough for it to be a substantial drop in population
      val death: Double = HumanPop.deathPropensity(birth,this.size,this.his/1000.0,this.his)
      (birth,death)

object HumanPop :

  def apply(
             resources: Double,
             s_res: Double
           ):
  Int =
    HumanPop(resources.toInt,s_res)
  def birth(
             size: Int
           ):
  Int =
    size + 1
  def death(
             size: Int
           ):
  Int =
    size - 1
  def birthPropensity(
                       i_val: Double
                     ):
  Double =
    i_val + 1.0
  def deathPropensity(
                       i_val: Double,
                       popSize: Int,
                       resources: Double,
                       his: Double,
                     ):
  Double =
    i_val + popSize*popSize/resources/his

  /**
   * Returns the type of Demographic event given a random number and the demographic propensities.
   *  @param x_rnd the random number thrown to sample the distributions.
   *  @param prop contains the birth and death propensities in field 1 and 2 respectively.
   *  @return the Demographic event type: Birth or Death.
   */
  def selectBirthOrDeath(
                          x_rnd: Double,
                          prop: (Double, Double)
                        ):
  EventType =
    x_rnd match {
      case x if x < prop._1 => EventType.Birth
      case x if x < prop._2 => EventType.Death
    }

end HumanPop
