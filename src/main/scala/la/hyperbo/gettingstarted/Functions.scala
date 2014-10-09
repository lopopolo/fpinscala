package la.hyperbo.gettingstarted

import scala.annotation.tailrec

object Functions {
  def fib(n: Int): Int = {
    @tailrec
    def go(i: Int, n1: Int, n2: Int): Int = {
      if (i == 0) n2
      else if (i == 1) n2 + n1
      else go(i - 1, n1 + n2, n1)
    }

    go(n, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = {
      if (i + 1 >= as.size) true
      else ordered(as(i), as(i + 1)) && go(i + 1)
    }

    go(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
