package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    def getQuadrant(loc: Location): (Double, Double, Double, Double) = {
      val d00 = grid(scala.math.ceil(loc.lat).toInt, scala.math.floor(loc.lon).toInt) // nw
      val d01 = grid(scala.math.floor(loc.lat).toInt, scala.math.floor(loc.lon).toInt) // sw
      val d10 = grid(scala.math.ceil(loc.lat).toInt, scala.math.ceil(loc.lon).toInt) // ne
      val d11 = grid(scala.math.floor(loc.lat).toInt, scala.math.ceil(loc.lon).toInt) // se
      (d00, d01, d10, d11)
    }
    def getPixel(x: Int, y: Int): Pixel = {
      import scala.math.abs
      val z = zoom + 8
      val loc = Interaction.tileLocation(z, x, y)
      val Location(lat, lon) = loc
      val (d00, d01, d10, d11) = getQuadrant(loc)
      val d00X = scala.math.floor(loc.lon).toInt
      val d00Y = scala.math.ceil(loc.lat).toInt
      val temperature = bilinearInterpolation(abs(lon - d00X), abs(lat - d00Y), d00, d01, d10, d11)
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

}
