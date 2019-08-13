package ds_algo.queue

import scala.reflect.ClassTag

/**
 * @author: Longlong He
 * @date: 2019/8/9 14:15
 * @description: 循环队列基于数组的实现
 * @reviewer: TODO 
 */
class CircularQueue[T: ClassTag](capacity: Int) extends DemoQueue[T] {

  var items = new Array[T](capacity)
  var head = 0
  var tail = 0

  /**
   * 入队考虑队满情况：(tail+1)%capacity=head
   *
   * @param data
   */
  override def enqueue(data: T): Unit = {
    require((tail + 1) % capacity != head, "queue is full")
    items(tail) = data
    tail = (tail + 1) % capacity
    size += 1
  }

  /**
   * 出队考虑队空情况：tail=head
   *
   * @return 队头的数据
   */
  override def dequeue(): Option[T] = {
    if (head == tail) {
      None
    } else {
      val result = Some(items(head))
      head = (head + 1) % capacity
      size -= 1
      result
    }
  }
}
