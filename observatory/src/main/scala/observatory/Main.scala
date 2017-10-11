package observatory

object Main extends App {

  val temperatureColors = Iterable(
    (60d, Color(255, 255, 255)),
    (32d, Color(255, 0, 0)),
    (12d, Color(255, 255, 0)),
    (0d, Color(0, 255, 255)),
    (-15d, Color(0, 0, 255)),
    (-27d, Color(255, 0, 255)),
    (-50d, Color(33, 0, 107)),
    (-60d, Color(0, 0, 0)))

  val deviationColors = Iterable(
    (7d, Color(0, 0, 0)),
    (4d, Color(255, 0, 0)),
    (2d, Color(255, 255, 0)),
    (0d, Color(255, 255, 255)),
    (-2d, Color(0, 255, 255)),
    (-7d, Color(0, 0, 255)))

  def printPerformance[T](caption: String, body: => T): T = {
    import System.currentTimeMillis

    println(caption + " started")

    val startTime = currentTimeMillis

    val res = body

    val endTime = (currentTimeMillis - startTime) / 1000
    println(caption + " end in " + endTime + " sec")

    res
  }

  def temperatureTileGeneration() = {

    def generateImage(year: Int, zoom: Int, x: Int, y: Int, temperatures: Iterable[(Location, Double)]): Unit = {
       val dir = "target/temperatures/" + year + "/" + zoom + "/"
       val path = new java.io.File(dir)
       this.synchronized {
         if (!path.exists) {
           assert(path.mkdirs, "Could not create directory: " + dir)
         }
       }
       val file = new java.io.File(dir + x + "-" + y + ".png")
       if (!file.exists) {
         val image = Interaction.tile(temperatures, temperatureColors, zoom, x, y)
         image.output(new java.io.File(dir + x + "-" + y + ".png"))
       }
    }
    def needCalculate(year: Int): Boolean = {
      val end: Int => Int = (zoom) => (1 << zoom) - 1
      for (zoom <- 0 to 3; x <- 0 to end(zoom); y <- 0 to end(zoom)) {
        val dir = "target/temperatures/" + year + "/" + zoom + "/"
        val file = new java.io.File(dir + x + "-" + y + ".png")
        if (!file.exists) {
          return true;
        }
      }
      return false;
    }
    def calculateAvgTemperatures(year: Int): (Int, Iterable[(Location, Double)]) = {
      val startTime = System.currentTimeMillis
      val temperatures = Extraction.locateTemperatures(year, "/stations.csv", "/" + year + ".csv")
      val avgTemperatures = Extraction.locationYearlyAverageRecords(temperatures)
      println(s"Avg. temperatures are calculated for $year in: " + ((System.currentTimeMillis - startTime) / 1000))
      (year, avgTemperatures)
    }

    for (year <- 1975 to 2015) {
      if (needCalculate(year)) {
        val yearTemperatures = calculateAvgTemperatures(year)
  
        println(s"Started generate tiles for $year")
        val startTime = System.currentTimeMillis
        Interaction.generateTiles[Iterable[(Location, Double)]](Iterable(yearTemperatures), generateImage)
        println(s"Finished tile generation in: " + ((System.currentTimeMillis - startTime) / 1000))
      }
    }
  }

  def temperatureDeviationTileGeneration() = {
    import scala.concurrent._
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    import Manipulation._
    import Extraction._
    import Visualization2._

    def calculateAvgTemperatures(year: Int): Iterable[(Location, Double)] = {
      val temperatures = locateTemperatures(year, "/stations.csv", "/" + year + ".csv")
      locationYearlyAverageRecords(temperatures)
    }
    def getNormals(years: Range): (Int, Int) => Double = {
      val avgTemperatureFutures = for (year <- years) yield Future(calculateAvgTemperatures(year))
      val avgTemperatures = avgTemperatureFutures.map(Await.result(_, Duration.Inf))
      average(avgTemperatures)
    }
    val normalGrid = printPerformance("Normals calculation", getNormals(1975 to 1989))
    for (year <- 1990 to 2015) {
      val end: Int => Int = (zoom) => (1 << zoom) - 1
      val futures = for (zoom <- 0 to 3; x <- 0 to end(zoom); y <- 0 to end(zoom)) yield
        Future {
          printPerformance(s"Generate image ${year}/${zoom}/${x}_${y}.png", {
            val dir = "target/deviations/" + year + "/" + zoom + "/"
            val path = new java.io.File(dir)
            this.synchronized {
              if (!path.exists) {
                assert(path.mkdirs, "Could not create directory: " + dir)
              }
            }
            val file = new java.io.File(dir + x + "-" + y + ".png")
            if (!file.exists) {
              val avgTemperatures = calculateAvgTemperatures(year)
              val deviationGrid = deviation(avgTemperatures, normalGrid)
              val image = visualizeGrid(deviationGrid, deviationColors, zoom, x, y)
              image.output(new java.io.File(dir + x + "-" + y + ".png"))
            }
          } )
        }
      futures.foreach(Await.result(_, Duration.Inf))
    }
  }

  //temperatureTileGenration; Uncomment if want to generate temperature tiles
  temperatureDeviationTileGeneration;

}
