import scala.annotation.tailrec

object ThreeSum extends Test {

  //TLE
  def v1(nums: Array[Int]): List[List[Int]] = {
    nums.toList.combinations(3).filter(_.sum == 0).toList
  }

  // TLE for all zero long list
  def v2(nums: Array[Int]): List[List[Int]] = {
    def sum2(input: Array[Int], target: Int): List[List[Int]] = {
      @tailrec
      def go(map: Map[Int, Int], i: Int, res: List[List[Int]]): List[List[Int]] = {
        if (i == input.length) res
        else if (map.contains(target - input(i))) {
          go(map, i + 1, List(target - input(i), input(i)) :: res)
        }
        else go(map + (input(i) -> i), i + 1, res)
      }

      go(Map.empty, 0, List.empty)
    }

    nums.indices.flatMap { i =>
      val res = sum2(nums.drop(i + 1), -nums(i))
      res.map(v => (nums(i) :: v).sorted)
    }.distinct.toList
  }

  def main(args: Array[String]): Unit = {
    v2(Array(-1, 0, 1, 2, -1, -4)) should contain theSameElementsAs List(List(-1, 0, 1), List(-1, -1, 2))
  }

}
