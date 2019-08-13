package ds_algo.queue

import scala.reflect.ClassTag

/**
 * @author: Longlong He
 * @date: 2019/8/9 14:51
 * @description: 基于数组且充分利用空间动态搬移数据的队列实现
 * @reviewer: TODO 
 */
class DynamicArrayQueue[T: ClassTag](val capacity: Int) extends ArrayQueue[T](capacity) {

  /**
   * tail=capacity 且数据还有空间时,应该搬移数据到数组的头部
   *
   * @param data 存储在下标为队尾的数据
   */
  override def enqueue(data: T): Unit = {
    if (tail == capacity) {
      require(head > 0, "queue is full")
      for (i <- Range(head, tail)) {
        items(i - head) = items(i)
      }
      tail = tail - head
      head = 0
    }
    super.enqueue(data)
  }
}
