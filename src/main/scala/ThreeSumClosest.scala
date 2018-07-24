// sum of three number closest to target
object ThreeSumClosest extends Test {
  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    val sorted = nums.sorted
    var result = 0
    var min = Int.MaxValue
    for (i <- sorted.indices) {
      var scd = i + 1
      var third = nums.length - 1
      while (scd < third) {
        val sum = sorted(i) + sorted(scd) + sorted(third)
        val diff = (sum - target).abs
        if (diff == 0) return target
        if (diff < min) {
          min = diff
          result = sum
        }
        if (sum < target) {
          scd += 1
        } else {
          third -= 1
        }
      }
    }
    result
  }

  def main(args: Array[String]): Unit = {
    threeSumClosest(Array(-1, 2, 1, -4), 1) should be(2)

  }
}
