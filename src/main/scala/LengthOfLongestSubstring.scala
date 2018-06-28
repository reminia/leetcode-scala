import scala.collection.immutable.HashSet
import scala.collection.mutable

object LengthOfLongestSubstring extends Test {
  //memory limit exceeded
  def lengthOfLongestSubstring(s: String): Int = {
    val res = for {
      i <- 0 until s.length
      j <- i + 1 to s.length
      substr = s.substring(i, j)
      if !repeat(substr, HashSet.empty)
    } yield j - i
    if (res.isEmpty) 0
    else res.max
  }

  def repeat(list: List[Char]): Boolean = {
    list.distinct.length != list.length
  }

  //another MLE
  def repeat(s: Seq[Char], set: HashSet[Char]): Boolean = {
    s match {
      case Seq()                             => false
      case Seq(a, t @ _*) if set.contains(a) => true
      case Seq(a, t @ _*)                    => repeat(t, set + a)
    }
  }

  def v2(s: String): Int = {
    var set: mutable.HashSet[Char] = mutable.HashSet.empty
    var i = 0
    var j = 0
    var res = 0
    while (i < s.length && j < s.length) {
      if (!set.contains(s(j))) {
        set += s(j)
        j = j + 1
        res = res.max(j - i)
      } else {
        set -= s(i)
        i = i + 1
      }
    }
    res
  }

  def v3(s: String): Int = {
    var i = 0
    var j = 0
    var res = 0
    var map: mutable.Map[Char, Int] = mutable.Map()
    while ( j < s.length) {
      if (!map.contains(s(j))) {
        map += s(j) -> j
        j = j + 1
        res = res.max(j - i)
      } else {
        if(map(s(j)) < i) {
          map += s(j) -> j
          j = j + 1
          res = res.max(j - i)
        } else {
          i = map(s(j)) + 1
          map += s(j) -> j
          j = j + 1
        }
      }
    }
    res
  }

  def v4(s: String): Int = {
    val map: mutable.Map[Char, Int] = mutable.Map()
    var i = 0
    var j = 0
    var res = 0
    while(j < s.length) {
      if(map.contains(s(j)) && map(s(j)) >= i) {
        i = map(s(j)) + 1
      } else {
        res = res.max(j + 1 - i)
      }
      map += s(j) -> j
      j = j + 1
    }
    res
  }

  def main(args: Array[String]): Unit = {
    lengthOfLongestSubstring("") should be(0)
    lengthOfLongestSubstring("a") should be(1)
    lengthOfLongestSubstring("aab") should be(2)
    v2("") should be(0)
    v2("a") should be(1)
    v2("aa") should be(1)
    v2("abcda") should be(4)
    v2("abcada") should be(4)
    v2("tmmzuxt") should be(5)

    v3("") should be(0)
    v3("a") should be(1)
    v3("aa") should be(1)
    v3("abcda") should be(4)
    v3("abcada") should be(4)
    v3("aabcd") should be(4)
    v3("tmmzuxt") should be(5)

    v4("tmmzuxt") should be(5)
    v4("aabcd") should be(4)
    v4("abcd") should be(4)
    v4("aa") should be(1)
  }
}
