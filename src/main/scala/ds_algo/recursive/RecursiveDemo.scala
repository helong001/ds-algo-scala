package ds_algo.recursive

import scala.collection.mutable

/**
 * @author: Longlong He
 * @date: 2019/8/9 16:16
 * @description: 使用递归求解问题需要满足的三个条件
 *               1）一个问题的解可以分解为多个子问题的解
 *               2）这个问题与分解后的子问题，除了数据规模不同，求解思路完全一样
 *               3）存在递归终止条件
 * @reviewer: TODO 
 */
object RecursiveDemo {

  val knownSteps = mutable.HashMap.empty[Int, Int]
  val knownResults = mutable.HashMap.empty[Int, Int]

  /**
   * 假有 n 个台阶，每次你可以跨 1 个台阶或者 2 个台阶。请问走这 n 个台阶有多少种走法？
   *
   * @param steps
   * @return
   */
  def calculateStepWays(steps: Int): Int = {
    steps match {
      case 1 => 1
      case 2 => 2
      case _ => knownSteps.get(steps) match {
        case Some(value) => value
        case None => {
          val result = calculateStepWays(steps - 1) + calculateStepWays(steps - 2)
          knownSteps.put(steps, result)
          result
        }
      }
    }
  }

  /**
   * 斐波那契数列 的递归实现 1 1 2 3 5 8
   *
   * @param n 斐波那契数列第几个数字
   * @return 第n个斐波那契数列中的数字
   */
  def fibonacciRecursive(n: Int): Int = {
    n match {
      case 1 => 1
      case 2 => 1
      case _ => knownResults.get(n) match {
        case Some(value) => value
        case None =>
          val result = fibonacciRecursive(n - 1) + fibonacciRecursive(n - 2)
          knownResults.put(n, result)
          result
      }
    }
  }

  /**
   * 斐波那契数列的尾递归实现
   *
   * @param n
   * @param result1 数列的第一个数字
   * @param result2 数列的第二个数字
   * @return
   */
  def fibonacciTailRecursive(n: Int, result1: Int, result2: Int): Int = {
    n match {
      case 1 => result1
      case _ => fibonacciTailRecursive(n - 1, result2, result1 + result2)
    }
  }

}
