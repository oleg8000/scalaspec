package observatory

import java.time.LocalDate
import scala.io._

/**
  * 1st milestone: data extraction
  */
object Extraction {

  type STN = String
  type WBAN = String
  type Station = ((STN, WBAN), Location)
  type Stations = Map[(STN, WBAN), Location]
  type Temperature = (LocalDate, Location, Double)

  def getStations(stationsFile: String): Stations = {
    def getStation(line: String): Option[Station] = {
      def getStation(arr: Array[String]): Option[Station] = {
        val stn = arr(0)
        val wban = arr(1)
        val latitude = arr(2)
        val longitude = arr(3)
        if (latitude.nonEmpty && longitude.nonEmpty) {
          Option(((stn, wban), Location(latitude.toDouble, longitude.toDouble)))
        } else {
          None
        }
      }
      val arr = line.split(",")
      if (arr.length >= 4) getStation(arr) else None
    }
    Source.
      fromInputStream(getClass.getResourceAsStream(stationsFile)).
      getLines.
      map(getStation(_)).
      filter(_.isDefined).
      map(_.get).
      toMap
  }
  def getTemperature(year: Int, stations: Stations, line: String): Option[Temperature] = {
    def getTemperature(arr: Array[String]): Option[Temperature] = {
      def F2C(temp: Double): Double = ((BigDecimal(temp) - 32) / 1.8).toDouble
      val stn = arr(0).trim
      val wban = arr(1).trim
      val stationId = (stn, wban)
      val month = arr(2).trim
      val day = arr(3).trim
      val temperatureF = arr(4).trim
      val location = stations.get(stationId)
      if (location.isDefined)
        Some((LocalDate.of(year, month.toInt, day.toInt), location.get, F2C(temperatureF.toDouble)))
      else
        None
    }
    val arr = line.split(",")
    if (arr.length >= 5) getTemperature(arr) else None
  }
  
  def locateTemperaturesSource(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = getStations(stationsFile)
    Source.
      fromInputStream(getClass.getResourceAsStream(temperaturesFile)).
      getLines.
      map(getTemperature(year, stations, _)).
      filter(_.isDefined).
      map(_.get).
      toIterable
  }

  def locateTemperaturesFuture(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    import java.lang.System._
    import scala.concurrent._
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    type TemperaturesChunk = List[Temperature]
    val chunkSize = 10000
    val stations = getStations(stationsFile)
    val chunks =
      Source.
      fromInputStream(getClass.getResourceAsStream(temperaturesFile)).
      getLines.
      grouped(chunkSize)
    var futures = List[Future[TemperaturesChunk]]()
    while(chunks.hasNext) {
      val lines = chunks.next
      val f: Future[TemperaturesChunk] = Future {
        val temperatures = lines.map(getTemperature(year, stations, _)).filter(_.isDefined).map(_.get)
        temperatures
      }
      futures = f :: futures
    }
    futures.flatMap(Await.result(_, Duration.Inf))
  }

  def locateTemperaturesSpark(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    import org.apache.spark.rdd.RDD
    import java.nio.file.Paths
    import java.lang.System._

    def fsPath(resource: String): String =
      Paths.get(this.getClass.getResource(resource).toURI).toString

    import org.apache.spark.sql._
    val spark: SparkSession =
      SparkSession
        .builder()
        .appName("Extraction")
        .config("spark.master", "local[3]")
        .getOrCreate()
    val stations = getStations(stationsFile)
    val lines = spark.sparkContext.textFile(fsPath(temperaturesFile), 3)
    lines
      .map(getTemperature(year, stations, _))
      .filter(_.isDefined)
      .map(_.get)
      .collect
      .toIterable
  }
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    locateTemperaturesSource(year, stationsFile, temperaturesFile)
    //locateTemperaturesSpark(year, stationsFile, temperaturesFile)
    //locateTemperaturesFuture(year, stationsFile, temperaturesFile)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    import java.lang.System._
    import scala.collection.parallel.ParIterable
    def avg(items: Iterable[(Location, Double)]): Double = {
      val temperatures = items.map { case (loc, temp) => temp }
      temperatures.sum / temperatures.size
    }
    records
      .map { case (_, loc, temp) => (loc, temp) }
      .groupBy { case (loc, temp) => loc }
      .mapValues(avg(_))
      .toList
  }

}
