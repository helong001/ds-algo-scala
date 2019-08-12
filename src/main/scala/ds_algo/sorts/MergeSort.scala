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

  private[this] def _merge(items: Array[Int], start: Int, pivot: Int, end: Int): Unit = {
    //前半部分的起点索引
    var i = start
    //后半部分的起点索引
    var j = pivot + 1
    //升序数组的索引
    var k = 0

    var lengthOne = j - i
    var lengthTwo = end - j + 1

    //用于合并升序后存储
    val sortedArray = new Array[Int](end - start + 1)

    while (i <= pivot || j <= end) {
      //<= 为了稳定排序，维持数据的前后顺序
      if (items(i) <= items(j) && i <=lengthOne) {
        sortedArray(k) = items(i)
        i += 1
      } else if (j <=lengthTwo) {
        sortedArray(k) = items(j)
        j += 1
      }
      k += 1
    }

    for (n <- 0 to end - start) {
      items(start + n) = sortedArray(n)
    }
  }
}
