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
      if(i > s.length/2) true
      else if (s(i) == s(s.length - i - 1)) f(i + 1)
      else false
    }
    f(0)
  }

  //DP
  def v2(s: String): String = {

    ???
  }

  def main(args: Array[String]): Unit = {
    longestPalindrome("aba") should be("aba")
    longestPalindrome("abcbd") should be("bcb")
    longestPalindrome("a") should be("a")
    longestPalindrome("") should be("")
    isPalindrome("aba") should be(true)
    isPalindrome("abc") should be(false)
    isPalindrome("a") should be(true)
  }
}
