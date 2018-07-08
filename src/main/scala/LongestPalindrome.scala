import scala.annotation.tailrec

object LongestPalindrome extends Test {
  //TLE
  def longestPalindrome(s: String): String = {
    val strs: Seq[String] = for {
      i <- 0 until s.length
      j <- i + 1 to s.length //to not until
      sub = s.substring(i, j)
      if sub == sub.reverse
    } yield {
      sub
    }
    if (strs.isEmpty) ""
    else
      strs.maxBy(_.length)
  }

  def isPalindrome(s: String): Boolean = {
    @tailrec
    def f(i: Int): Boolean = {
      if (i > s.length / 2) true
      else if (s(i) == s(s.length - i - 1)) f(i + 1)
      else false
    }
    f(0)
  }

  //DP without memorization
  //dp(i)(j) => whether s(i to j) is palindrome
  def v2(s: String): String = {
    def dp(i: Int, j: Int): Boolean = {
      (i, j) match {
        case _ if i == j     => true
        case _ if j == i + 1 => s(i) == s(j)
        case _               => dp(i + 1, j - 1) && s(i) == s(j)
      }
    }
    val list = for {
      i <- s.indices
      j <- i until s.length
      if dp(i, j)
    } yield (i, j)

    if (list.isEmpty) {
      s
    } else {
      val (x, y) = list.maxBy { case (i, j) => j - i }
      s.substring(x, y + 1)
    }
  }

  //DP with memorization
  // arr(i)(j) => where arr(i)(j) is palindrome
  def v4(s: String): String = {
    val arr = Array.ofDim[Boolean](s.length, s.length)
    for {
      step <- s.indices
      i <- s.indices
      if i + step < s.length
    } {
      val j = i + step
      if (step == 0) arr(i)(j) = true
      else if (step == 1) arr(i)(j) = s(i) == s(j)
      else arr(i)(j) = arr(i + 1)(j - 1) && s(i) == s(j)
    }

    var max = ""
    for {
      i <- s.indices
      j <- s.indices
      if arr(i)(j)
    } {
      if (j - i + 1 > max.length) {
        max = s.substring(i, j + 1)
      }
    }
    max
  }

  //even faster DP
  def v5(s: String): String = {
    ""
  }

  // TLE => same as v1 but use sliding to enumerate string
  def v6(s: String): String = {
    val list = for {
      i <- 1 to s.length
      str <- s.sliding(i)
      if isPalindrome(str)
    } yield str
    if (s.isEmpty) s
    else {
      list.maxBy(_.length)
    }
  }

  // Improve on v6
  // 1. enumeration begin from longest substring
  // 2. use view to lazily compute
  // TLE again
  def v7(s: String): String = {
    if (s.isEmpty) s
    else
      (for {
        i <- (s.length to 1 by -1).view
        str <- s.sliding(i)
        if isPalindrome(str)
      } yield str).head
  }

  //center based validation
  def v3(s: String): String = {
    val length = s.length

    @tailrec
    def f(str: String, left: Int, right: Int): String = {
      if (left >= 0 && right < length && s(left) == s(right)) {
        f(s.substring(left, right + 1), left - 1, right + 1)
      } else {
        str
      }
    }

    def odd(i: Int): String = {
      f("", i, i)
    }

    def even(i: Int): String = {
      f("", i, i + 1)
    }

    s.indices.foldLeft[String]("") {
      case (res, i) =>
        List(res, odd(i), even(i)).maxBy(_.length)
    }

  }

  def main(args: Array[String]): Unit = {
    longestPalindrome("aba") should be("aba")
    longestPalindrome("abcbd") should be("bcb")
    longestPalindrome("a") should be("a")
    longestPalindrome("") should be("")

    isPalindrome("aba") should be(true)
    isPalindrome("abc") should be(false)
    isPalindrome("a") should be(true)

    v3("aba") should be("aba")
    v3("abcbd") should be("bcb")
    v3("a") should be("a")
    v3("") should be("")

    v2("aba") should be("aba")
    v2("abcbd") should be("bcb")
    v2("a") should be("a")
    v2("") should be("")
    v2("abcba") should be("abcba")

    v4("aba") should be("aba")
    v4("abcbd") should be("bcb")
    v4("a") should be("a")
    v4("") should be("")
    v4("abcba") should be("abcba")

    v6("aba") should be("aba")
    v6("abcbd") should be("bcb")
    v6("a") should be("a")
    v6("") should be("")
    v6("abcba") should be("abcba")

    v7("aba") should be("aba")
    v7("abcbd") should be("bcb")
    v7("a") should be("a")
    v7("") should be("")
    v7("abcba") should be("abcba")
  }
}
