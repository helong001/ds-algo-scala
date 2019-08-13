package ds_algo.sorts

/**
 * @author: Longlong He
 * @date: 2019/8/12 20:08
 * @description: 快速排序
 * @reviewer: TODO 
 */
object QuickSort {

  def quickSort(items: Array[Int]): Array[Int] = {
    _quickSort(items, 0, items.length - 1)
    items
  }

  def findKthElement(items: Array[Int], k: Int): Int = {
    _findKthElement(items, k, 0, items.length - 1)
  }

  private[this] def _findKthElement(items: Array[Int], k: Int, start: Int, end: Int): Int = {
    val partitionIndex = _partition(items, start, end)
    if (k == partitionIndex + 1) {
      items(partitionIndex)
    } else if (k < partitionIndex + 1) {
      _findKthElement(items, k, start, partitionIndex - 1)
    } else {
      _findKthElement(items, k, partitionIndex + 1, end)
    }
  }

  private[this] def _quickSort(items: Array[Int], start: Int, end: Int): Unit = {
    if (start >= end) return
    val partitionIndex = _partition(items, start, end)
    _quickSort(items, start, partitionIndex - 1)
    _quickSort(items, partitionIndex + 1, end)
  }

  private[this] def _getPivotIndex(items: Array[Int], start: Int, end: Int): Int = {
    val pivotIndex = (start + end) / 2
    //简单的排序，也可以用插入排序
    if (items(pivotIndex) < items(start)) _swap(items, pivotIndex, start)
    if (items(pivotIndex) > items(end)) _swap(items, pivotIndex, end)
    if (items(pivotIndex) < items(start)) _swap(items, pivotIndex, start)
    pivotIndex
  }

  private[this] def _swap(items: Array[Int], firstIndex: Int, secondIndex: Int): Unit = {
    var temp = items(firstIndex)
    items(firstIndex) = items(secondIndex)
    items(secondIndex) = temp
  }

  private[this] def _partition(items: Array[Int], start: Int, end: Int): Int = {
    //val pivotIndex = _getPivotIndex(items, start, end)
    val pivot = items(end)
    var i = start
    for (j <- start to end) {
      if (items(j) < pivot) {
        _swap(items, i, j)
        i += 1
      }
    }
    _swap(items, i, end)
    i
  }
}
