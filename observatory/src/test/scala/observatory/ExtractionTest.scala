package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
/*
  test("test test data") {
    val temperatures = Extraction.locateTemperatures(2015, "/teststations.csv", "/testtemperatures.csv")
    val avg = Extraction.locationYearlyAverageRecords(temperatures)
    println( "Result: " + avg.toList )
  }
  */
  test("test real data") {
    val temperatures = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val records = Extraction.locationYearlyAverageRecords(temperatures)
    println("Record count: " + records.size)
  }
}