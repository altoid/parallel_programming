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
    if (chars.length == 0) true
    else {
      var stack = 0
      for (c <- chars) {
        if (c == '(') stack = stack + 1
        else if (c == ')') stack = stack - 1
        if (stack < 0) return false
      }
      stack == 0
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      // returns a pair of ints.  first is number of unmatched left parens,
      // second is number of unmatched right parens.

      var stack = List[Char]()

      for (i <- idx until until) {
        if (chars(i) == '(') {
          stack = chars(i) :: stack
        }
        else if (chars(i) == ')') {
          stack = stack match {
            case '(' :: t => stack.tail
            case _ => ')' :: stack
          }
        }
      }
      val (left, right) = stack.partition(_ == '(')
      (left.length, right.length)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val mid = from + (until - from) / 2
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))

        val matched_in_both = l._1 min r._2
        (l._1 + r._1 - matched_in_both, l._2 + r._2 - matched_in_both)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
