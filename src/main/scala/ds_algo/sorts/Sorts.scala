package ds_algo.sorts

import scala.util.control.Breaks.{break, breakable}

/**
 * @author: Longlong He
 * @date: 2019/8/12 15:50
 * @description: 经典排序算法: bubble sort,insertion sort,selection sort
 *               小数据量，O(n^^2)，都是原地排序
 *               选择排序不是稳定排序，冒泡和插入排序是稳定排序
 *               1）元素比较
 *               2）元素交换
 * @reviewer: TODO 
 */
object Sorts {

  /**
   * 冒泡排序
   *
   * 时间复杂度：最好O(n) 最坏和平均都是O(n^^2)
   * 空间复杂度：O(1)
   * 稳定排序
   * 每次比较都需要数据交换并且都是三次赋值操作
   *
   * @param items 未排序数组
   * @return 已排序数组
   */
  def bubbleSort(items: Array[Int]): Array[Int] = {
    val length = items.length
    breakable {
      for (i <- 0 until length) {
        var swapped = false
        for (j <- 0 until length - i - 1) {
          if (items(j) > items(j + 1)) {
            val temp = items(j)
            items(j) = items(j + 1)
            items(j + 1) = temp
            swapped = true
          }
        }
        if (!swapped) break
      }
    }
    items
  }

  /**
   * 插入排序
   *
   * 时间复杂度 最好O(n) 最坏和平均都是O(n^^2)
   * 空间复杂度 O(1)
   * 稳定排序
   * 每次比较都需要数据交换且是一次赋值操作，只有找到插入的位置后才为三次赋值操作
   *
   * @param items 未排序数组
   * @return 已排序数组
   */
  def insertionSort(items: Array[Int]): Array[Int] = {
    val length = items.length
    if (length <= 1) return items
    for (i <- 1 until length) {
      var toBeComparedItem = items(i)
      var j = i - 1
      breakable {
        while (j >= 0) {
          if (items(j) > toBeComparedItem) {
            items(j + 1) = items(j)
            j -= 1
          } else {
            break
          }
        }
      }
      items(j + 1) = toBeComparedItem
    }
    items
  }

  /**
   * 选择排序
   *
   * 时间复杂度 最好最坏都是O(n^^2)
   * 空间复杂度 O(1)
   * 不稳定排序-数据交换破坏了稳定性
   *
   * @param items 未排序数组
   * @return 已排序数组
   */
  def selectionSort(items: Array[Int]): Array[Int] = {
    val length = items.length
    for (i <- 0 until length) {
      var indexOfMin = getIndexOfMin(items, i)
      var toBeSwapped = items(i)
      items(i) = items(indexOfMin)
      items(indexOfMin) = toBeSwapped
    }
    items
  }

  /**
   * 获取数组中索引范围内最小值的索引
   *
   * @param items      待查找的数组
   * @param beginIndex 起始索引
   * @return 最小值的索引值
   */
  def getIndexOfMin(items: Array[Int], beginIndex: Int): Int = {
    val length = items.length
    var indexOfMin = beginIndex
    for (i <- beginIndex until length) {
      if (items(i) < items(indexOfMin)) {
        indexOfMin = i
      }
    }
    indexOfMin
  }
}
