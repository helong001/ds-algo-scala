package ds_algo.stack

sealed class Node[T](var data: T, var next: Option[Node[T]])

class StackDemo[T] {
  var headOpt: Option[Node[T]] = None
  var size = 0

  /**
   * 清空栈
   */
  def clear(): Unit = {
    headOpt = None
    size = 0
  }

  /**
   * 将数据data 压入栈顶
   *
   * @param data
   */
  def push(data: T): Unit = {
    val newHead = new Node(data, headOpt)
    headOpt = Some(newHead)
    size += 1
  }

  /**
   * 将栈顶出栈，如果为空则返回空值
   *
   * @return
   */
  def pop(): Option[Node[T]] = {
    headOpt match {
      case None => None
      case Some(head) =>
        headOpt = head.next
        size -= 1
        Some(head)
    }
  }
}
