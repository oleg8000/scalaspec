package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    import scala.math._
    val lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << zoom).toDouble))))
    val lon = x.toDouble / (1 << zoom).toDouble * 360.0 - 180.0
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    import com.sksamuel.scrimage.Pixel
    import scala.concurrent._
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global

    def getPixel(x: Int, y: Int): Pixel = {
      val z = zoom + 8
      val loc = tileLocation(z, x, y)
      val temperature = Visualization.predictTemperature(temperatures, loc)
      val color = Visualization.interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, alpha = 127)
    }
    val tileW, tileH = 256
    val pointStartX = x * tileW
    val pointStartY = y * tileH
    val pointEndX = pointStartX + tileW
    val pointEndY = pointStartY + tileH
    val pixels = for {
      pointY <- Array.range(pointStartY, pointEndY)
      pointX <- Array.range(pointStartX, pointEndX)
    } yield getPixel(pointX, pointY)

    Image(tileW, tileH, pixels)
  }
  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    import scala.concurrent._
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    def yearlyDataProcessing(yearAndData: (Int, Data)): List[Future[Unit]] = {
      val (year, data) = yearAndData
      val end: Int => Int = (zoom) => (1 << zoom) - 1
      val futures = for (zoom <- 0 to 3; x <- 0 to end(zoom); y <- 0 to end(zoom)) yield
        Future {
          generateImage(year, zoom, x, y, data)
        }
      futures.toList
    }
    val futures = for (yd <- yearlyData) yield yearlyDataProcessing(yd)
    futures.flatten.foreach(Await.result(_, Duration.Inf))
  }

}
