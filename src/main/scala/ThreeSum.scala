import scala.annotation.tailrec
import scala.collection.mutable

object ThreeSum extends Test {

  //TLE
  def v1(nums: Array[Int]): List[List[Int]] = {
    nums.toList.combinations(3).filter(_.sum == 0).toList
  }

  // TLE for all zero long list
  def v2(nums: Array[Int]): List[List[Int]] = {
    def sum2(input: Array[Int], target: Int): List[List[Int]] = {
      @tailrec
      def go(map: Map[Int, Int],
             i: Int,
             res: List[List[Int]]): List[List[Int]] = {
        if (i == input.length) res
        else if (map.contains(target - input(i))) {
          go(map, i + 1, List(target - input(i), input(i)) :: res)
        } else go(map + (input(i) -> i), i + 1, res)
      }

      go(Map.empty, 0, List.empty)
    }

    nums.indices
      .flatMap { i =>
        val res = sum2(nums.slice(i + 1, nums.length), -nums(i))
        res.map(v => (nums(i) :: v).sorted)
      }
      .distinct
      .toList
  }

  // O(n^2) AC
  def v3(nums: Array[Int]): List[List[Int]] = {
    val res = new mutable.ListBuffer[List[Int]]
    val sorted = nums.sorted
    for (i <- sorted.indices) {
      if (i == 0 || sorted(i) > sorted(i - 1)) {
        var end = nums.length - 1
        var scd = i + 1
        while (scd < end) {
          if (sorted(i) + sorted(scd) + sorted(end) == 0) {
            res += List(sorted(i), sorted(scd), sorted(end))
            scd += 1
            end -= 1
            while (scd < end && sorted(scd) == sorted(scd - 1)) scd += 1
            while (scd < end && sorted(end) == sorted(end + 1)) end -= 1
          } else if (sorted(i) + sorted(scd) + sorted(end) < 0) {
            scd += 1
          } else {
            end -= 1
          }
        }
      }
    }
    res.toList
  }

  def main(args: Array[String]): Unit = {
    v2(Array(-1, 0, 1, 2, -1, -4)) should contain theSameElementsAs List(
      List(-1, 0, 1),
      List(-1, -1, 2))

    v3(Array(-1, 0, 1, 2, -1, -4)) should contain theSameElementsAs List(
      List(-1, 0, 1),
      List(-1, -1, 2))
  }

}
