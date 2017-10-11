package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 1000000 
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i,count: Int = 0
    while ((count >= 0) && (i < chars.length)) {
      if (chars(i) == '(')
        count = count + 1
      else if (chars(i) == ')')
        count = count - 1
      i = i + 1
    }
    if (count == 0) true else false
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int/*, arg1: Int, arg2: Int*/): (Int, Int)/*: ???*/ = {
      var countOpen, countClose = 0
      var i = from
      while (i < until) {
        if (chars(i) == '(')
          countOpen = countOpen + 1
        else if (chars(i) == ')') {
          if (countOpen>0)
            countOpen = countOpen - 1
          else
            countClose = countClose + 1
        }
        i = i + 1
      }
      (countClose, countOpen)
    }

    def reduce(from: Int, until: Int): (Int, Int)/*: ???*/ = {
      if ((until - from) <= threshold)
        traverse(from, until)
      else {
        val middle = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, middle), reduce(middle,until))
        (left._1, (left._2 - right._1) + right._2)
      }
    }

    reduce(0, chars.length) == (0, 0)/*???*/
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
