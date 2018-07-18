//string to integer
object Atoi extends Test {

  //WA => for input number may be larger than long
  //AC after modification
  def atoi(str: String): Int = {
    def f(ss: String, sign: Int): Int = {
      if (ss.isEmpty) 0
      else if (!ss(0).isDigit) 0
      else {
        val d = ss.dropWhile(_ == '0').takeWhile(_.isDigit).take(11)
        if (d.isEmpty) 0
        else {
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
    }

    val s = str.dropWhile(_ == ' ')
    if (s.isEmpty) 0
    else {
      s(0) match {
        case '+'             => f(s.tail, 1)
        case '-'             => f(s.tail, -1)
        case a if !a.isDigit => 0
        case _               => f(s, 1)
      }
    }

  }

  def v2(str: String): Int = {
    def pos(s: String): Int = {
      if (s.isEmpty) 0
      else if (s.length < 10) s.toInt
      else if (s.length > 10) Int.MaxValue
      else if (s.length == 10 && s < Int.MaxValue.toString) s.toInt
      else Int.MaxValue
    }
    def neg(s: String): Int = {
      if (s.isEmpty) 0
      else if (s.length < 10) -s.toInt
      else if (s.length > 10) Int.MinValue
      else if (s.length == 10 && s < Int.MinValue.toString.tail) -s.toInt
      else Int.MinValue
    }

    def digits(s: String): String = {
      s.dropWhile(_ == '0').takeWhile(_.isDigit)
    }

    val s = str.dropWhile(_ == ' ')
    if (s.isEmpty || s(0) != '+' && s(0) != '-' && !s(0).isDigit)
      0
    else {
      s(0) match {
        case '+' => pos(digits(s.tail))
        case '-' => neg(digits(s.tail))
        case _ =>
          pos(digits(s))
      }
    }
  }

  // a fast traverse solution with mutable variables
  def v3(str: String): Int = {
    var i = 0
    var begin = true
    var sum = 0L
    var isPositive = true
    0
  }

  def main(args: Array[String]): Unit = {
    atoi("a12") should be(0)
    atoi(" \t") should be(0)
    atoi("+") should be(0)
    atoi("-") should be(0)
    atoi(" +12abc") should be(12)
    atoi(" 12abc") should be(12)
    atoi(" -12abc") should be(-12)
    atoi("-91283472332") should be(Int.MinValue)
    atoi("2147483649") should be(Int.MaxValue)
    atoi("000123") should be(123)
    atoi(" 00000 ") should be(0)

    v2("a12") should be(0)
    v2(" \t") should be(0)
    v2("+") should be(0)
    v2("-") should be(0)
    v2(" +12abc") should be(12)
    v2(" 12abc") should be(12)
    v2(" -12abc") should be(-12)
    v2("-91283472332") should be(Int.MinValue)
    v2("2147483649") should be(Int.MaxValue)
    v2("000123") should be(123)
    v2(" 00000 ") should be(0)
  }

}
