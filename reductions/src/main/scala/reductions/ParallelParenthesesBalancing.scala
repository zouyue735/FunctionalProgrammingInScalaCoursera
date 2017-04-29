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
    var currentCount = 0
    var idx = 0
    while (currentCount >= 0 && idx < chars.length) {
      val inc = chars(idx) match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
      currentCount += inc
      idx += 1
    }
    currentCount == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until) (arg1, arg2)
      else {
        var cur = idx
        var left = arg1
        var right = arg2
        while (cur < until) {
          val inc = chars(cur) match {
            case '(' => 1
            case ')' => -1
            case _ => 0
          }
          left += inc

          if (left < 0) {
            right += 1
            left = 0
          }

          cur += 1
        }
        (left, right)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        val a = traverse(from, until, 0, 0)
        print(a)
        a
      }
      else {
        val mid = (until + from) / 2
        val (p1, p2) = parallel(reduce(from, mid), reduce(mid, until))
        val balanced = Math.min(p1._1, p2._2)
        (p1._1 - balanced + p2._1, p1._2 + p2._2 - balanced)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
