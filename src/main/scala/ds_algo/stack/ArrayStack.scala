package ds_algo.stack

import scala.reflect.ClassTag

class ArrayStack[T: ClassTag](capacity: Int) {
  val items = new Array[T](capacity)
  var count = 0

  /**
   * 将item压入栈中
   *
   * @param item 需要压入栈中的数据
   * @return 成功则返回true
   */
  def push(item: T): Boolean = {
    //如果栈满了，就返回false
    if (count == capacity) return false
    //压入数据，count加一
    items(count) = item
    count += 1
    true
  }

  /**
   * 出栈操作
   *
   * @return
   */
  def pop(): T = {
    if (count == 0) None
    val item = items(count - 1)
    count -= 1
    item
  }

  /**
   * 清空栈
   */
  def clear(): Unit = {
    count = 0
  }
}
