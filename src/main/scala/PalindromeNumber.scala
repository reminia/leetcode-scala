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

  def v3(x: Int): Boolean = {
    if (x < 0 || x % 10 == 0 && x != 0) false
    else {
      var y = x
      var res = 0
      while (y != 0) {
        res = res * 10 + y % 10
        y = y / 10
      }
      res == x
    }
  }

  def v4(x: Int): Boolean = {
    if (x < 0 || x % 10 == 0 && x != 0) false
    else {
      var y = x
      var res = 0
      while (y > res) {
        res = res * 10 + y % 10
        y = y / 10
      }
      res == y || res / 10 == y
    }
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

    v3(-1) should be(false)
    v3(10) should be(false)
    v3(1) should be(true)
    v3(11) should be(true)
    v3(12) should be(false)
    v3(121) should be(true)
    v3(123) should be(false)

    v4(-1) should be(false)
    v4(10) should be(false)
    v4(1) should be(true)
    v4(11) should be(true)
    v4(12) should be(false)
    v4(121) should be(true)
    v4(123) should be(false)
  }

}
