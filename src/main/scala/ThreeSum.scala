object ThreeSum {

  //TLE
  def v1(nums: Array[Int]): List[List[Int]] = {
    nums.toList.combinations(3).filter(_.sum == 0).toList
  }

  def main(args: Array[String]): Unit = {}

}
