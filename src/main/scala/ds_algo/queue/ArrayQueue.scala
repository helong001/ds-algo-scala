package ds_algo.queue

import scala.reflect.ClassTag

class ArrayQueue[T: ClassTag](capacity: Int) extends DemoQueue[T] {

  var items = new Array[T](capacity)
  var head = 0
  var tail = 0

  /**
   * 入队操作-先进先出
   *
   * @param data 存储在下标为队尾的数据
   */
  override def enqueue(data: T): Unit = {
    require(tail < capacity, "queue is full")
    items(tail) = data
    tail += 1
    size += 1
  }

  /**
   * 出队操作
   * 1)head <tail 队未空
   *
   * @return 下标为队头的数据
   */
  override def dequeue(): Option[T] = {
    if (head < tail) {
      val result = Some(items(head))
      head += 1
      size -= 1
      result
    } else {
      None
    }
  }
}
