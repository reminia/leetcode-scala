import scala.annotation.tailrec

object ReverseInteger extends Test {
  def reverse(x: Int): Int = {
    try {
      val y = x.abs.toString.reverse.toInt
      if (x < 0) -y
      else y
    } catch {
      case _: Exception => 0
    }
  }

  // case match faster then if else
  def v2(x: Int): Int = {
    try {
      x match {
        case Int.MinValue => 0
        case _ if x < 0   => -x.abs.toString.reverse.toInt
        case _            => x.abs.toString.reverse.toInt
      }
    } catch {
      case _: Exception => 0
    }
  }

  def v3(x: Int): Int = {
    val pos = Int.MaxValue / 10
    val neg = Int.MinValue / 10

    @tailrec
    def f(v: Int, r: Int): Int = {
      r match {
        case _ if v == 0                  => r
        case _ if r > pos                 => 0
        case _ if r == pos && v % 10 > 7  => 0
        case _ if r < neg                 => 0
        case _ if r == neg && v % 10 < -8 => 0
        case _ =>
          val pop = v % 10
          f(v / 10, r * 10 + pop)
      }
    }
    f(x, 0)
  }

  def main(args: Array[String]): Unit = {
    reverse(123) should be(321)
    reverse(10) should be(1)
    reverse(-12) should be(-21)
    reverse(0) should be(0)
    reverse(-10) should be(-1)
    reverse(Int.MinValue) should be(0)
    reverse(Int.MaxValue) should be(0)

    v2(123) should be(321)
    v2(10) should be(1)
    v2(-12) should be(-21)
    v2(0) should be(0)
    v2(-10) should be(-1)
    v2(Int.MinValue) should be(0)
    v2(Int.MaxValue) should be(0)

    v3(123) should be(321)
    v3(10) should be(1)
    v3(-12) should be(-21)
    v3(0) should be(0)
    v3(-10) should be(-1)
    v3(Int.MinValue) should be(0)
    v3(Int.MaxValue) should be(0)

  }

}
