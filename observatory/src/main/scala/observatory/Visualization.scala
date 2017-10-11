package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    import scala.math._
    def distance(p1: Location, p2: Location): Double = {
      val p1LatRad = toRadians(p1.lat)
      val p2LatRad = toRadians(p2.lat)
      val p1LonRad = toRadians(p1.lon)
      val p2LonRad = toRadians(p2.lon)
      val d = acos(sin(p1LatRad) * sin(p2LatRad) + cos(p1LatRad) * cos(p2LatRad) * cos(abs(p1LonRad - p2LonRad)))
      val earthRadius = 6371d
      d * earthRadius
    }
    def w(distance: Double, p: Double = 4d): Double = 1d / pow(distance, p)

    val ltdIterable = temperatures.map { case (l, t) => (l, t, distance(l, location)) }
    val ltd = ltdIterable.find { case (_, _, d) => if (d < 1d) true else false }
    if (ltd.isDefined) {
      val (l, t, d) = ltd.get
      t
    } else {
      type Sums = Tuple2[Double, Double]
      type LTD = Tuple3[Location, Double, Double]
      def calc(sums: Sums, ltd: LTD): Sums = {
        val (numerator, denominator) = sums
        val (_, t, d) = ltd
        val weight = w(d)
        (numerator + weight * t, denominator + weight)
      }
      val (numerator, denominator) = ltdIterable.foldLeft((0d, 0d))(calc(_, _))
      numerator / denominator
    }
  }
  implicit class ColorOps(val c1: Color) {
    def -(c2: Color): Color = Color(c1.red - c2.red, c1.green - c2.green, c1.blue - c2.blue)
    def +(c2: Color): Color = Color(c1.red + c2.red, c1.green + c2.green, c1.blue + c2.blue)
    def *(v: Double): Color = {
      val red = c1.red * v
      val green = c1.green * v
      val blue = c1.blue * v
      Color(red.round.toInt, green.round.toInt, blue.round.toInt)
    }
  }
  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    import scala.util.Sorting._
    import scala.reflect._
    import scala.math._
    import scala.collection.Searching._

    def interpolateColor(point1: (Double, Color), point2: (Double, Color)): Color = {
      val x: Double = value
      val x1: Double = point1._1
      val x2: Double = point2._1
      val fX1: Color = point1._2
      val fX2: Color = point2._2
      fX1 + (fX2 - fX1) * ((x - x1) / (x2 - x1))
    }
    val sortedPoints = stableSort(points.toSeq)(classTag[(Double, Color)], Ordering.by(_._1))
    val searchResult = sortedPoints.search((value, Color(0, 0, 0)))(Ordering.by(_._1))
    searchResult match {
      case Found(idx) => sortedPoints(idx)._2
      case InsertionPoint(idx) => {
        assert(sortedPoints.nonEmpty)
        if (sortedPoints(0)._1 > value)
          sortedPoints(0)._2
        else if (sortedPoints(sortedPoints.length - 1)._1 < value)
          sortedPoints(sortedPoints.length - 1)._2
        else
          interpolateColor(sortedPoints(idx - 1), sortedPoints(idx))
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    import scala.math._
    import scala.util.Sorting._
    import scala.reflect._
    import scala.collection.Searching._
    import com.sksamuel.scrimage.Pixel
    val sortedColors = stableSort(colors.toSeq)(classTag[(Double, Color)], Ordering.by(_._1))
    val blackPixel = Pixel(0, 0, 0, 0)
    val pixels = Array.fill[Pixel](360 * 180)(blackPixel)

    def FillPixels(temp: (Location, Double)): Unit = {
      val (location, temperature) = temp
      val x = (location.lon + 180).toInt
      val y = abs(location.lat - 90).toInt
      require(x >= 0)
      require(x < 360)
      require(y >= 0)
      require(y < 180)
      val searchResult = sortedColors.search((temperature, Color(0, 0, 0)))(Ordering.by(_._1))
      val color = searchResult match {
        case Found(idx) => sortedColors(idx)._2
        case InsertionPoint(idx) => throw new NoSuchElementException("Color is not found")
      }
      val idx = y * 360 + x
      pixels(idx) = Pixel(color.red, color.green, color.blue, 0)
    }

    temperatures.foreach(FillPixels(_))
    Image(360, 180, pixels)
  }

}

