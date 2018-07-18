object PalindromeNumber extends Test {
  def v1(x: Int): Boolean = {
    val s = x.toString
    s == s.reverse
  }

  def v2(x: Int): Boolean = {
    val s = x.toString
    val len = s.length
    def f(i: Int): Boolean = {
      if (i > len / 2) true
      else if (s(i) == s(len - i - 1)) f(i + 1)
      else false
    }
    f(0)
  }

  def main(args: Array[String]): Unit = {
    v1(1) should be(true)
    v1(11) should be(true)
    v1(12) should be(false)
    v1(123) should be(false)

    v2(1) should be(true)
    v2(11) should be(true)
    v2(12) should be(false)
    v2(123) should be(false)
  }

}
