package algo

/**
  * Given an array of integers, return indices of the two numbers such that they add up to a specific target.
  *
  * You may assume that each input would have exactly one solution, and you may not use the same element twice.
  *
  * Example:
  *
  * Given nums = [2, 7, 11, 15], target = 9,
  *
  * Because nums[0] + nums[1] = 2 + 7 = 9,
  * return [0, 1].
  *
  */

import scala.collection.mutable.HashMap

object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val numsMap = new HashMap[Int, Int]();
    var result = new Array[Int](2)
    for (i <- 0 to nums.length - 1) {
      val anotherIndex = numsMap.get(target - nums(i));
      if (None != anotherIndex) result = Array(i, anotherIndex.get);
      numsMap.put(nums(i), i)
    }
    result
  }
}