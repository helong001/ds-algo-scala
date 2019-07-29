package ds_algo.array

class ArrayList[T](capacity: Int) {
  var data: Array[T] = new Array[T](capacity)
  var length: Int = 0

  /**
   * 通过下标随机访问数组的值
   * @param index
   * @return
   */
  def find(index: Int): T = {
    require(index >= 0 && index < length, "index should be between 0 and length of array!")
    data(index)
  }

  /**
   * 数组的插入操作，从后往index移动数据
   * 考虑数组容量扩容情况
   * @param index
   * @param value
   * @return
   */
  def insert(index: Int, value: T): Boolean = {
    /**
     * 数组扩容2倍：三种方式
     * 1）Arrays.copyOf
     * 2)System.arraycopy
     * 3)遍历复制
     */
    if (length == capacity) {
      var temp = new Array[T](capacity * 2)
      for (i <- 0 to length - 1) {
        temp(i) = data(i)
      }
      data = temp
    }
    require(index >= 0 && index < capacity, "index must be between 0 and capacity of array!")
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
   * @param index
   * @return
   */
  def delete(index: Int): T = {
    require(length != 0, "array is empty")
    require(index >= 0 && index < length, "index must be between 0 and length of array")
    val result = data(index)
    for (i <- index until length - 1) {
      data(i) = data(i + 1)
    }
    length -= 1
    result
  }
}
