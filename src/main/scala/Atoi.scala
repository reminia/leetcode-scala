//string to integer
object Atoi extends Test {

  //WA => for input number may be larger than long
  def atoi(str: String): Int = {
    def f(ss: String, sign: Int): Int = {
      if (ss.isEmpty) 0
      else if (!ss(0).isDigit) 0
      else {
        val d = ss.takeWhile(_.isDigit)
        val l = d.toLong
        if (sign > 0) {
          if (l > Int.MaxValue) Int.MaxValue
          else l.toInt
        } else {
          if (-l < Int.MinValue) Int.MinValue
          else -l.toInt
        }
      }
    }

    val s = str.dropWhile(_ == ' ')
    if (s.isEmpty) 0
    else {
      s(0) match {
        case '+'             => f(s.drop(1), 1)
        case '-'             => f(s.drop(1), -1)
        case a if !a.isDigit => 0
        case _               => f(s, 1)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    atoi("a12") should be(0)
    atoi(" \t") should be(0)
    atoi(" 12abc") should be(12)
    atoi(" -12abc") should be(-12)
    atoi("-91283472332") should be(Int.MinValue)
    atoi("2147483649") should be(Int.MaxValue)
  }

}
