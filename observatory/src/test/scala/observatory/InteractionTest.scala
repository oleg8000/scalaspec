package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("Generate image 256x256") {
    var startTime = System.currentTimeMillis
    val temperatures = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    println("Extraction.locateTemperatures completed in : " + (System.currentTimeMillis - startTime))
    startTime = System.currentTimeMillis
    val avgTemperatures = Extraction.locationYearlyAverageRecords(temperatures)
    println("Extraction.locationYearlyAverageRecords completed in : " + (System.currentTimeMillis - startTime))
    /*
    startTime = System.currentTimeMillis
    val locs = for (x <- 1 to 100000) yield Interaction.tileLocation(1, x, 3)
    println("Interaction.tileLocation completed in : " + (System.currentTimeMillis - startTime))
    println("Loc count collected: " + locs.length)
    */
    val colors = Iterable(
      (60d, Color(255, 255, 255)),
      (32d, Color(255, 0, 0)),
      (12d, Color(255, 255, 0)),
      (0d, Color(0, 255, 255)),
      (-15d, Color(0, 0, 255)),
      (-27d, Color(255, 0, 255)),
      (-50d, Color(33, 0, 107)),
      (-60d, Color(0, 0, 0)))
    startTime = System.currentTimeMillis
    Interaction.tile(avgTemperatures, colors, zoom = 0, x = 0, y = 0).output(new java.io.File("target/some-image.png"))
    println("Interaction.tile completed in : " + (System.currentTimeMillis - startTime))
    assert(true)
  }

/*
  test("Fast image generate 256x256") {
    val temperatures = Extraction.locateTemperatures(2021, "/teststations1.csv", "/2021.csv")
    val avgTemperatures = Extraction.locationYearlyAverageRecords(temperatures)
    val colors = Iterable(
      (60d, Color(255, 255, 255)),
      (32d, Color(255, 0, 0)),
      (12d, Color(255, 255, 0)),
      (0d, Color(0, 255, 255)),
      (-15d, Color(0, 0, 255)),
      (-27d, Color(255, 0, 255)),
      (-50d, Color(33, 0, 107)),
      (-60d, Color(0, 0, 0)))
    Interaction.tile(avgTemperatures, colors, zoom = 0, x = 0, y = 0).output(new java.io.File("target/some-image.png"))
    assert(true)
  }
*/
}
