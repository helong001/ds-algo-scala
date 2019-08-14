package ds_algo.binary_search

/**
 * @author: Longlong He
 * @date: 2019/8/14 10:13
 * @description: TODO
 * @reviewer: TODO 
 */
object BinarySearch {

  def findFirstValue(items: Array[Int], value: Int): Int = {
    0
  }

  def findLastValue(items: Array[Int], value: Int): Int = {
    0
  }

  def findFirstGreaterThan(items: Array[Int], value: Int): Int = {
    0
  }

  def findLastSmallerThan(items: Array[Int], value: Int): Int = {
    0
  }

  def search(items: Array[Int], value: Int): Int = {
    _search(items, value)
  }

  private[this] def _search(items: Array[Int], value: Int): Int = {
    var low = 0
    var high = items.length - 1

    while (low <= high) {
      val mid = low + ((high - low) >> 1)
      if (items(mid) == value) {
        return mid
      } else if (items(mid) < value) {
        low = mid + 1
      } else {
        high = mid - 1
      }
    }
    -1
  }

  def searchRecursive(items: Array[Int], value: Int): Int = {
    _searchRecursive(items, value, 0, items.length - 1)
  }

  private[this] def _searchRecursive(items: Array[Int], value: Int, low: Int, high: Int): Int = {
    if (low > high) return -1
    val mid = low + ((high - low) >> 1)
    if (items(mid) == value) {
      mid
    } else if (items(mid) < value) {
      _searchRecursive(items, value, mid + 1, high)
    } else {
      _searchRecursive(items, value, low, high - 1)
    }
  }
}
