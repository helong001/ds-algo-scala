package ds_algo.linkedlist

/**
 * 1) 单链表反转
 * 2) 链表中环的检测
 * 3) 两个有序的链表合并
 * 4) 删除链表倒数第n个结点
 * 5) 求链表的中间结点
 * author: Longlong He
 */
object LinkedListAlgo {

  /**
   * 反转链表
   *
   * @param head
   * @return
   */
  def reverse(head: Node): Node = {
    require(head != null, "the head of linked list is null")
    if (head.next.isEmpty) {
      return head
    }

    var pre: Option[Node] = None
    var next: Option[Node] = None
    var current: Option[Node] = Some(head)
    //遍历链表结点并反转链表方向，当前结点不是尾结点时，反转方向并移动当前结点
    while (current.get.next.isDefined) {
      next = current.get.next
      current.get.next = pre

      pre = current
      current = next
    }
    //当前结点为为结点，反转方向
    current.get.next = pre

    current.get
  }

  /**
   * 找到链表环的入口
   *
   * @param head
   * @return
   */
  def findCircleEntrance(head: Node): Option[Node] = {
    checkCircle(head).map(node => {
      val length = calculateCircleLength(node)
      var fast = head
      var slow = head
      //结点比一个结点多走环长的步数然后开始同步后移，再次相遇的结点就是环的入口
      for (i <- 0 until length) {
        fast = fast.next.get
      }

      while (slow != fast) {
        fast = fast.next.get
        slow = slow.next.get
      }
      slow
    })
  }

  /**
   * 检测链表中是否有环
   *
   * @param head
   * @return Some(node) a node in a circle or None
   */
  def checkCircle(head: Node): Option[Node] = {
    var fast = head
    var slow = head

    while (fast.next.isDefined && fast.next.get.next.isDefined) {
      fast = fast.next.get.next.get
      slow = slow.next.get

      if (fast.equals(slow)) {
        return Some(slow)
      }
    }
    None
  }

  /**
   * 根据检测链表是否有环的结果，计算环的长度
   *
   * @param node -some node in the circle
   * @return circle length
   */
  def calculateCircleLength(node: Node): Int = {
    var length = 1
    var cursor = node.next.get
    while (cursor != node) {
      length += 1
      cursor = cursor.next.get
    }
    length
  }

}
