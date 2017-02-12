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
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
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
    def updateCount(c: Char, openCount: Int): Int = {
      if (c == '(') openCount + 1
      else if (c == ')') openCount - 1
      else openCount
    }

    def trackedBalance(chars: Array[Char], index: Int, openCount: Int): Boolean = {
      if (openCount < 0) false
      else if (chars.isEmpty || index >= chars.length) openCount == 0
      else trackedBalance(chars, index + 1, updateCount(chars(index), openCount))
    }

    trackedBalance(chars, 0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, leftOpen: Int, leftClosed: Int): (Int, Int) = {
      if (idx == until) (leftOpen, leftClosed)
      else {
        var (i, lo, lc) = (idx, leftOpen, leftClosed)
        while (i < until) {
          if (chars(i) == '(') lo += 1
          else if (chars(i) == ')') {
            if (lo > 0) lo -= 1 else lc += 1
          }
          i += 1
        }
        (lo, lc)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (from == until)
        (0, 0)
      else if (until - from <= 1 || threshold <= 0 || until - from < threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((lo1, lc1), (lo2, lc2)) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        if (lo1 >= lc2) (lo1 - lc2 + lo2, lc1)
        else if (lo1 > 0) (lo2, lc1 + lc2 - lo1)
        else (lo1 + lo2, lc1 + lc2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
