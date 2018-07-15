import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

trait Test extends FlatSpec with Matchers {
  def time[T](x: => T): Unit = {
    val start = System.currentTimeMillis()
    x
    val end = System.currentTimeMillis()
    println(end - start)
  }
}

object ZZZ extends Test {
  def z(x: Int): Int = {
    x * 2
  }
  def f(n: Int): Unit = {
    val flag: Array[Boolean] = Array.ofDim[Boolean](n)
    val values: Array[Int] = Array.ofDim[Int](n)
    (0 until n).foreach { x =>
      if (flag(x)) values(x)
      else {
        flag(x) = true
        values(x) = z(x)
      }

    }
    (0 until n).foreach {
      values(_)
    }
  }
  def g(n:Int): Unit = {
    val map: mutable.Map[Int, Int] = mutable.Map.empty
    (0 until n) foreach { x =>
      map.getOrElseUpdate(x, z(x))
    }
    (0 until n).foreach {
      map(_)
    }
  }

  def main(args: Array[String]): Unit = {
    time(f(10000))
    println("-------")
    time(g(10000))

  }
}
