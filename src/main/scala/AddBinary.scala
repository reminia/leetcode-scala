import scala.collection.mutable

object AddBinary extends Test {
  def value(s: String): Long = {
    var sum = 0L
    val size = s.length - 1
    for (i <- s.length - 1 to 0 by -1) {
      if (s(i) == '1') sum += 1 << (size - i)
    }
    sum
  }

  def binary(s: Long): String = {
    val sb = new StringBuilder
    var n = s
    while (n / 2 != 0) {
      sb.append(n % 2)
      n = n / 2
    }
    sb.append(n % 2)
    sb.toString.reverse
  }

  // string to value, then add, then value to binary string
  // WA: long number's finite length will cause string truncation
  def addBinary(a: String, b: String): String = {
    val sum = value(a) + value(b)
    binary(sum)
  }

  //emulate plus of binary numbers
  def v2(c: String, d: String): String = {
    var res: mutable.ListBuffer[Char] = mutable.ListBuffer.empty
    var i = 0
    var flag = 0
    val a = c.reverse
    val b = d.reverse
    while (i < a.length || i < b.length) {
      var sum = flag
      if (i < a.length) {
        sum += a(i).toString.toInt
      }
      if (i < b.length) {
        sum += b(i).toString.toInt
      }
      if (sum == 2) {
        '0' +=: res
        flag = 1
      } else if (sum == 3) {
        '1' +=: res
        flag = 1
      } else if (sum == 1) {
        '1' +=: res
        flag = 0
      } else {
        '0' +=: res
        flag = 0
      }
      i += 1
    }
    if (flag == 1)
      '1' +=: res
    res.mkString
  }

  def main(args: Array[String]): Unit = {
    value("110") should be(6)
    value("111") should be(7)
    value("01") should be(1)
    binary(4) should be("100")
    binary(3) should be("11")
    addBinary("1", "1") should be("10")
    addBinary("10", "1") should be("11")
    addBinary("10", "10") should be("100")
    v2("1", "1") should be("10")
    v2("10", "1") should be("11")
    v2("10", "10") should be("100")
  }
}
