package wikipedia

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

import org.apache.spark.rdd.RDD

case class WikipediaArticle(title: String, text: String)

object WikipediaRanking {

  val langs = List(
    "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
    "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  val conf: SparkConf = new SparkConf().setMaster( "local[2]" ).setAppName( "wikipedia" )
  val sc: SparkContext = new SparkContext( conf )
  // Hint: use a combination of `sc.textFile`, `WikipediaData.filePath` and `WikipediaData.parse`
  val wikiRdd: RDD[WikipediaArticle] = sc.textFile(WikipediaData.filePath).map(WikipediaData.parse(_))

  /** Returns the number of articles on which the language `lang` occurs.
   *  Hint1: consider using method `aggregate` on RDD[T].
   *  Hint2: should you count the "Java" language when you see "JavaScript"?
   *  Hint3: the only whitespaces are blanks " "
   *  Hint4: no need to search in the title :)
   */
  def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int = {
    def isMentioned(lang: String, text: String): Boolean =
      text.split(" ").exists(_ == lang)
    rdd.aggregate(0)(
      (count, wa) => if (isMentioned(lang, wa.text)) count + 1 else count,
      _ + _)
  }

  /* (1) Use `occurrencesOfLang` to compute the ranking of the languages
   *     (`val langs`) by determining the number of Wikipedia articles that
   *     mention each language at least once. Don't forget to sort the
   *     languages by their occurence, in decreasing order!
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
  def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] = {
    def insertLang(rank: (String, Int), ranks: List[(String, Int)]): List[(String, Int)] =
      if (ranks.isEmpty) {
        List[(String, Int)](rank)
      }
      else {
        val (lang, count) = rank
        val (headLang, headCount) = ranks.head
        if (headCount < count)
          rank :: ranks
        else
          ranks.head :: insertLang(rank, ranks.tail)
      }
    langs.foldLeft(List[(String, Int)]())((ranks, lang) =>
      insertLang((lang, occurrencesOfLang(lang, rdd)), ranks))
  }

  /* Compute an inverted index of the set of articles, mapping each language
   * to the Wikipedia pages in which it occurs.
   */
  def makeIndex(langs: List[String], rdd: RDD[WikipediaArticle]): RDD[(String, Iterable[WikipediaArticle])] = {
    def langsMentionedInWikipediaArticle(wa: WikipediaArticle): List[(String, WikipediaArticle)] = {
      val words = wa.text.split(" ")
      def langExists(lang: String): Boolean = words.exists(_ == lang)
      for (lang <- langs; if langExists(lang)) yield (lang, wa)
    }
    rdd.flatMap(langsMentionedInWikipediaArticle(_)).groupByKey
  }
      

  /* (2) Compute the language ranking again, but now using the inverted index. Can you notice
   *     a performance improvement?
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
  def rankLangsUsingIndex(index: RDD[(String, Iterable[WikipediaArticle])]): List[(String, Int)] =
    index.mapValues(_.size).collect.toList.sortWith((x1, x2) => x1._2 > x2._2)

  /* (3) Use `reduceByKey` so that the computation of the index and the ranking is combined.
   *     Can you notice an improvement in performance compared to measuring *both* the computation of the index
   *     and the computation of the ranking? If so, can you think of a reason?
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
  def rankLangsReduceByKey(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] = {
    def langsMentionedInWikipediaArticle(wa: WikipediaArticle): List[(String, Int)] = {
      val words = wa.text.split(" ")
      def langExists(lang: String): Boolean = words.exists(_ == lang)
      for (lang <- langs; if langExists(lang)) yield (lang, 1)
    }
    rdd.flatMap(langsMentionedInWikipediaArticle(_)).reduceByKey(_ + _).collect.toList.sortWith((x1, x2) => x1._2 > x2._2)
  }

  def main(args: Array[String]) {

    /* Languages ranked according to (1) */
    val langsRanked: List[(String, Int)] = timed("Part 1: naive ranking", rankLangs(langs, wikiRdd))

    /* An inverted index mapping languages to wikipedia pages on which they appear */
    def index: RDD[(String, Iterable[WikipediaArticle])] = makeIndex(langs, wikiRdd)

    /* Languages ranked according to (2), using the inverted index */
    val langsRanked2: List[(String, Int)] = timed("Part 2: ranking using inverted index", rankLangsUsingIndex(index))

    /* Languages ranked according to (3) */
    val langsRanked3: List[(String, Int)] = timed("Part 3: ranking using reduceByKey", rankLangsReduceByKey(langs, wikiRdd))

    /* Output the speed of each ranking */
    println(timing)
    sc.stop()
  }

  val timing = new StringBuffer
  def timed[T](label: String, code: => T): T = {
    val start = System.currentTimeMillis()
    val result = code
    val stop = System.currentTimeMillis()
    timing.append(s"Processing $label took ${stop - start} ms.\n")
    timing.append(result + "\n")
    result
  }
}
