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

  def main(args: Array[String]): Unit = {
    twoSum(Array(2, 2, 1, 4), 4) shouldEqual Array(0, 1)
  }
}
