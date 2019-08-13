package ds_algo.sorts

/**
 * @author: Longlong He
 * @date: 2019/8/12 18:05
 * @description: 归并排序
 *               先对数组进行分解，直到不能再分解
 *               对分解的数组进行合并操作同时排序
 *               任何情况下，时间复杂度都为 O(nlogn)
 *               空间复杂度 O(n)
 *               是稳定排序
 * @reviewer: TODO 
 */
object MergeSort {

  def mergeSort(items: Array[Int]): Array[Int] = {
    _mergeSort(items, 0, items.length - 1)
    items
  }

  private[this] def _mergeSort(items: Array[Int], start: Int, end: Int): Unit = {
    if (start >= end) return
    val pivot = (start + end) / 2
    _mergeSort(items, start, pivot)
    _mergeSort(items, pivot + 1, end)
    _merge(items, start, pivot, end)
  }

  /**
   * 利用哨兵简化合并操作 (整数的最大值放在前后部分末尾)
   *
   * @param items 待排序数组
   * @param start 待排序起点
   * @param pivot 待排序中点
   * @param end   待排序终点
   */
  private[this] def _merge(items: Array[Int], start: Int, pivot: Int, end: Int): Unit = {

    val firstLength = pivot - start + 1
    val secondLength = end - pivot
    val firstPart = new Array[Int](firstLength + 1)
    val secondPart = new Array[Int](secondLength + 1)

    for (i <- 0 until firstLength) {
      firstPart(i) = items(start + i)
    }
    firstPart(firstLength) = Int.MaxValue

    for (i <- 0 until secondLength) {
      secondPart(i) = items(pivot + i + 1)
    }
    secondPart(secondLength) = Int.MaxValue

    var i = 0
    var j = 0
    for (k <- start to end) {
      if (firstPart(i) <= secondPart(j)) {
        items(k) = firstPart(i)
        i += 1
      } else {
        items(k) = secondPart(j)
        j += 1
      }
    }
  }
}
