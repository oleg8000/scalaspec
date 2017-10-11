package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {
import Visualization._

  test("ColorsOps-") {
    val c1 = Color(10, 20, 30)
    val c2 = Color(5, 10, 15)
    val cResult = c1 - c2
    val cSubtraction = c2
    assert(cResult == cSubtraction)
  }

  test("ColorsOps+") {
    val c1 = Color(10, 20, 30)
    val c2 = Color(5, 10, 15)
    val cSum = Color(15, 30, 45)
    val cResult = c1 + c2
    assert(cResult == cSum)
  }

  test("ColorsOps*") {
    val c1 = Color(10, 20, 30)
    val cMultiplication = Color(13, 27, 40)
    val cResult = c1 * 1.333
    assert(cResult == cMultiplication)
  }

  test("interpolateColor") {
    val points = Iterable((10.0, Color(255, 255, 255)), (20.0, Color(255, 0, 0)), (30.0, Color(255, 255, 0)))
    val value = 25
    val result = interpolateColor(points, value)
    assert(result == Color(255, 128, 0))
  }

  test("interpolateColor2") {
    val points = Iterable((30.0, Color(255, 255, 0)), (20.0, Color(255, 0, 0)), (10.0, Color(255, 255, 255)))
    val value = 20
    val result = interpolateColor(points, value)
    assert(result == Color(255, 0, 0))
  }

  test("interpolateColor3") {
    val points = Iterable((10.0, Color(255, 255, 255)), (20.0, Color(255, 0, 0)), (30.0, Color(255, 255, 0)))
    val value = 35.0
    val result = interpolateColor(points, value)
    assert(result == Color(255, 255, 0))
  }

  test("interpolateColor4") {
    val points = Iterable((30.0, Color(255, 255, 0)), (20.0, Color(255, 0, 0)), (10.0, Color(255, 255, 255)))
    val value = 35.0
    val result = interpolateColor(points, value)
    assert(result == Color(255, 255, 0))
  }

  test("visualize") {
    import com.sksamuel.scrimage.Pixel
    val loc1 = Location(90, -180)
    val x1y1 = (0, 0)
    val loc2 = Location(0, 0)
    val x2y2 = (180, 90)
    val loc3 = Location(-89, 179)
    val x3y3 = (359, 179)
    val col = Color(255, 255, 0)
    val temperatures = Iterable((loc1, 12.0), (loc2, 12.0), (loc3, 12.0))
    val colors = Iterable((12.0, col))
    val image = visualize(temperatures, colors)
    val pixel = Pixel(255, 255, 0, 0)
    val blackPixel = Pixel(0, 0, 0, 0)

    def CheckPixelCorrectness(x: Int, y: Int, curPixel: Pixel): Boolean = {
      val xy = (x, y)
      if ((x1y1 == xy) || (x2y2 == xy) || (x3y3 == xy)) {
        curPixel == pixel
      } else {
        curPixel == blackPixel
      }
    }
    assert(image.forall(CheckPixelCorrectness(_, _, _)))
  }

  test("predictTemperature") {
    import scala.math._
    val temp = predictTemperature(List((Location(45.0, -90.0), -100.0), (Location(-45.0, 0.0), 1.0)), Location(90.0, -180.0))
    val d1 = abs(-100.0 - temp)
    val d2 = abs(1.0 - temp)
    assert(d1 < d2)
  }

  test("predictTemperature2") {
    import scala.math._
    val temp = predictTemperature(List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0)), Location(88.0, -176.0))
    val d1 = abs(10.0 - temp)
    val d2 = abs(20.0 - temp)
    assert(d1 < d2)
  }

}
