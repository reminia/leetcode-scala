import scala.annotation.tailrec
import scala.collection.mutable

object TwoSum extends Test {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val array = for {
      i <- nums.indices
      j <- (i + 1) until nums.length
      if nums(j) + nums(i) == target
    } yield (i, j)
    val (a, b) = array.head
    Array(a, b)
  }

  // two pass hash table
  def v2(nums: Array[Int], target: Int): Array[Int] = {
    val map = nums.zipWithIndex.toMap
    nums.zipWithIndex.view.collect {
      new PartialFunction[(Int, Int), Array[Int]] {
        override def isDefinedAt(x: (Int, Int)): Boolean = {
          val remain = target - x._1
          val index = map.getOrElse(remain, -1)
          index != -1 && index != x._2
        }

        override def apply(v1: (Int, Int)): Array[Int] =
          Array(v1._2, map(target - v1._1))
      }
    }.head
  }

  // one pass hash table
  def v3(nums: Array[Int], target: Int): Array[Int] = {
    val map: mutable.Map[Int, Int] = mutable.Map.empty
    for ((v, i) <- nums.zipWithIndex) {
      val r = target - v
      if (map.contains(r))
        return Array(i, map(r))
      else map += v -> i
    }
    Array(0, 0)
  }

  // one pass hash table without use  any return
  def v4(nums: Array[Int], target: Int): Array[Int] = {
    @tailrec
    def go(i: Int, map: Map[Int, Int]): Array[Int] = {
      if (map.contains(target - nums(i))) {
        Array(map(target - nums(i)), i)
      } else {
        go(i + 1, map + (nums(i) -> i))
      }
    }
    go(0, Map.empty)
  }

  def main(args: Array[String]): Unit = {
    twoSum(Array(2, 2, 1, 4), 4) shouldEqual Array(0, 1)
    twoSum(Array(2, 2, 1, 4), 3) shouldEqual Array(0, 2)

    v2(Array(2, 2, 1, 4), 4) shouldEqual Array(0, 1)
    v2(Array(2, 2, 1, 4), 3) shouldEqual Array(0, 2)

    v4(Array(2, 2, 1, 4), 4) shouldEqual Array(0, 1)
    v4(Array(2, 2, 1, 4), 3) should (be(Array(0, 2)) or be(Array(1, 2)))
  }
}
