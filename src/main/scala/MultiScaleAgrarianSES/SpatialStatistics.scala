package MultiScaleAgrarianSES

import scala.jdk.CollectionConverters.SetHasAsScala

import org.jgrapht.*
import org.jgrapht.graph.*
import org.jgrapht.alg.util.NeighborCache


trait SpatialStatistics:

  val composition: Map[Long, EcoUnit]
  val neighborCacheStats: NeighborCache[Long, DefaultEdge]


  /*
  * Function to calculate spatial autocorrelation based on landcover.
  * */
  def spatialAutocorrelationMoranI(targetCover: LandCover):
  Double =
    val (above,below): (Double,Double) = composition.foldLeft[(Double,Double)]((0.0,0.0)){

      case ( (sumOut, wNOut),  (id, eu) )  =>

        val thisCoverTarget : Boolean = eu.matchCover(targetCover)

        val (sum, weight): (Double,Double) = neighborCacheStats.neighborsOf(id).asScala.toSet.foldLeft[(Double,Double)]((0.0,0.0)){

          case ( (sumIn, wNIn), n ) =>

              val neighborCoverTarget : Boolean = composition.getOrElse(n, EcoUnit(-1L,LandCover.NoCover)).matchCover(targetCover)

              if thisCoverTarget == neighborCoverTarget
              then (sumIn+1.0, wNIn+1.0)
              else (sumIn-1.0, wNIn+1.0)

        }

        (sumOut + sum, wNOut + weight)
    }
    above / below

  /**
   * Spatial autocorrelation of continuous observable.
   */
  def spatialAutocorrelationMoranI(
                                    obs: Map[Long,Double]
                                  ):
  Double =

    val average = obs.values.sum / obs.size.toDouble

    val (crossSum, autoSum, wSum): (Double, Double, Double) = obs.foldLeft[(Double, Double,Double)]((0.0, 0.0, 0.0)) {

      case ((crossOut, auto, wOut), (id, obsVal1)) =>
        val diff = obsVal1 - average
        val (crossIn, wIn): (Double, Double) = neighborCacheStats.neighborsOf(id).asScala.toSet.foldLeft[(Double, Double)]((0.0, 0.0)) {

          case ((sum, w), nid) =>
            val obsVal2 = obs.getOrElse(nid,0.0)
            (sum + diff * ( obsVal2 - average ), w + 1.0)

        }

        (crossOut + crossIn, auto + diff*diff  , wOut + wIn)
    }
    obs.size.toDouble * crossSum / (autoSum * wSum)


  def globalPearsonCorrelation(
                                obs1: Map[Long,Double],
                                obs2: Map[Long,Double]
                              ):
  Double =

   def calculateMapAverage(m: Map[Long,Double]): Double = m.values.sum / m.size.toDouble

   def calculateMapDispersionAroundAverage(m: Map[Long,Double]):
   Map[Long,Double] =
     val avg: Double = calculateMapAverage(m)
     m.map( v => (v._1, v._2 - avg) )

   def calculateMapVariance(m: Map[Long,Double]):
   Double =
     val squaredDispersion = calculateMapDispersionAroundAverage(m).map(v => (v._1, v._2 * v._2))
     calculateMapAverage(squaredDispersion)

   val d1 : Map[Long,Double] = calculateMapDispersionAroundAverage(obs1)
   val d2 : Map[Long,Double] = calculateMapDispersionAroundAverage(obs2)

   val correlationVector: Map[Long,Double] = d1.map{
     case (k1,v1) =>
       val v2 = d2.getOrElse(k1,0.0)
       (k1,v1*v2)
   }

   val covariance: Double = calculateMapAverage(correlationVector)

   covariance / math.sqrt(calculateMapVariance(obs1)) / math.sqrt(calculateMapVariance(obs2))




  /*
  * TODO: this function has an error in the calculation of moran's I when there are more than 2 categories
  * */
  def spatialAutocorrelationMoranDiagram:
  Map[Double,Double] =

    composition.map{
      case (l, unit) =>
        val avg: Double = neighborCacheStats.neighborsOf(l).asScala.toSet.foldLeft[Double](0.0){
          case (sum, n) =>
            if unit.matchCover(composition.getOrElse(n, EcoUnit(-1L, LandCover.NoCover)).cover)
            then sum + 1.0
            else sum - 1.0
        }/neighborCacheStats.neighborsOf(l).asScala.toSet.size.toDouble
        if unit.matchCover(LandCover.Natural) then (1.0,avg) else (-1.0,avg)
    }







