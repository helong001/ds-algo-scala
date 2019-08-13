package ds_algo.queue

sealed class Node[T](var data: T, var next: Option[Node[T]])

/**
 * 基于链表的队列操作实现
 */
class LinkedListQueue[T]() extends DemoQueue[T] {

  var headOpt: Option[Node[T]] = None
  var tailOpt: Option[Node[T]] = None

  /**
   * 入队操作
   *
   * @param data
   */
  override def enqueue(data: T): Unit = {
    val newItem = new Node(data, None)
    size += 1
    if (headOpt.isEmpty) {
      headOpt = Some(newItem)
    }

    if (tailOpt.isDefined) {
      tailOpt.get.next = Some(newItem)
    }
    tailOpt = Some(newItem)
  }

  /**
   * 出队操作
   *
   * @return
   */
  override def dequeue(): Option[T] = {

    if (size == 0) {
      tailOpt = None
      return None
    }
    size -= 1
    val head = headOpt.get
    headOpt = head.next
    Some(head.data)
  }
}
