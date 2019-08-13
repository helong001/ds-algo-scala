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
      //快结点比慢结点多走环长的步数然后开始同步后移，再次相遇的结点就是环的入口
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

  /**
   * 合并两个有序链表
   *
   * @param nodeA
   * @param nodeB
   * @return 合并后的链表头结点
   */
  def mergeSortedList(nodeA: Option[Node], nodeB: Option[Node]): Option[Node] = {
    var preHead = new Node(-1, None)
    var pre = preHead
    var headA = nodeA
    var headB = nodeB
    while (headA.isDefined && headB.isDefined) {
      if (headA.get.data <= headB.get.data) {
        pre.next = headA
        headA = headA.get.next
      } else {
        pre.next = headB
        headB = headB.get.next
      }
      pre = pre.next.get
    }
    //必然会先遍历完其中一个链表（二选一），余下的链表合并到尾部
    if (headA.isDefined) {
      pre.next = headA
    } else {
      pre.next = headB
    }
    preHead.next
  }

  /**
   * 删除链表倒数第 k个结点
   *
   * @param headOpt
   * @param k
   * @return 新链表的头结点
   */
  def deleteLastKthNode(headOpt: Option[Node], k: Int): Option[Node] = {
    require(k > 0, "K must be greater than 0")
    headOpt match {
      case None => None
      case Some(head) =>
        var preHead = Option(new Node(-1, None))
        preHead.get.next = headOpt

        var fast = preHead
        var slow = preHead

        //让快结点先于慢结点 k步
        for (i <- 0 until k + 1) {
          if (fast.isEmpty) {
            throw new IllegalArgumentException("given linked list should contains at least k elements")
          } else {
            fast = fast.get.next
          }
        }

        while (fast.isDefined) {
          fast = fast.get.next
          slow = slow.get.next
        }
        //删除第K个结点
        slow.get.next = slow.get.next.get.next
        preHead.get.next
    }
  }

  def mkStringForChain(node: Node): String = {
    val result = new StringBuilder
    var p = node
    while (p.next.isDefined) {
      result.append(p.data)
      p = p.next.get
    }
    result.append(p.data)
    result.mkString
  }
}
