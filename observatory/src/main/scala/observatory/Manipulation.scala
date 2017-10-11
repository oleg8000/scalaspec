package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    import Visualization._

    var grid = Map[(Int, Int), Double]()

    for (lat <- (-89 to 90); lon <- (-180 to 179)) {
      grid = grid + ((lat, lon) -> predictTemperature(temperatures, Location(lat, lon)))
    }

    (lat: Int, lon: Int) => {
      val latitude =
        if (lat < -89) {
          -89
        } else if (lat > 90) {
          90
        } else {
          lat
        }
      val longitude = 
        if (lon < -180) {
          -180
        } else if (lon > 179) {
          179
        } else {
          lon
        }
      val temperature = grid.get((latitude, longitude))
      assert(temperature.isDefined, s"Could not find temperature with latitude $lat and longitude $lon.")
      temperature.get
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    import scala.concurrent._
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global

    val grids = temperaturess.map(x => Future(makeGrid(x))).map(Await.result(_, Duration.Inf))
    (lat: Int, lon: Int) => {
      val predictedTemperatures = grids.map(_(lat, lon))
      predictedTemperatures.sum / predictedTemperatures.size
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(temperatures)
    (lat: Int, lon: Int) => {
      grid(lat, lon) - normals(lat, lon)
    }
  }


}

