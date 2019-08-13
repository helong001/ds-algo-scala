package ds_algo.array

class ArrayDemo(capacity: Int) {
  var data: Array[Char] = new Array[Char](capacity)
  var length: Int = 0

  /**
   * 通过下标随机访问数组的值
   *
   * @param index
   * @return
   */
  def find(index: Int): Char = {
    require(index >= 0 && index < length, "index should be between 0 and length of ds_algo.array!")
    data(index)
  }

  /**
   * 数组的插入操作，从后往index移动数据
   * 考虑数组容量扩容情况
   *
   * @param index
   * @param value
   * @return
   */
  def insert(index: Int, value: Char): Boolean = {
    /**
     * 数组扩容2倍：三种方式
     * 1）Arrays.copyOf
     * 2)System.arraycopy
     * 3)遍历复制
     */
    //    if (length == capacity) {
    //      var temp = new Array[Char](capacity * 2)
    //      for (i <- 0 to length - 1) {
    //        temp(i) = data(i)
    //      }
    //      data = temp
    //    }

    if (length == capacity) {
      return false
    }
    require(index >= 0 && index < capacity, "index must be between 0 and capacity of ds_algo.array!")
    for (i <- length until index by -1) {
      data(i) = data(i - 1)
    }
    data(index) = value
    length += 1
    true
  }

  /**
   * 数组的删除操作，从index往后移动数据
   * 考虑数组为空情况
   *
   * @param index
   * @return
   */
  def delete(index: Int): Char = {
    require(length != 0, "ds_algo.array is empty")
    require(index >= 0 && index < length, "index must be between 0 and length of ds_algo.array")
    val result = data(index)
    for (i <- index until length - 1) {
      data(i) = data(i + 1)
    }
    length -= 1
    result
  }

  def print: String = {
    data.subSequence(0, length).toString
  }
}
