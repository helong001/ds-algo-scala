package ds_algo.linkedlist

import scala.util.control.Breaks._

/**
 * the model class for the linked list
 *
 * @param data
 * @param next
 */
sealed case class Node(var data: Int, var next: Option[Node])

class SinglyLinkedList(var headOpt: Option[Node]) {

  // define constructor without param
  def this() = this(None)

  def insertToHead(value: Int): Unit = {
    val newNode = new Node(value, None)
    insertToHead(newNode)
  }

  def insertTail(value: Int): Unit = {
    val newNode = new Node(value, None)
    insertTail(newNode)
  }

  /**
   * 将新结点插入到尾部(新的尾结点的下一结点是否必须为None?)
   * 需要校验链表中是否有环
   *
   * @param newNode
   */
  def insertTail(newNode: Node): Unit = {
    headOpt match {
      case None =>
        //如果链表为空，将node作为头结点
        headOpt = Some(newNode)
      case Some(head) =>
        //找到尾结点
        //需要考虑单链表中是否有环
        var node = head
        while (node.next.isDefined) {
          node = node.next.get
        }
        //将新结点设置为尾结点
        node.next = Some(newNode)
    }
  }

  def insertAfter(existNode: Node, value: Int): Unit = {
    val newNode = new Node(value, None)
    insertAfter(existNode, newNode)
  }

  /**
   * 将新结点插入已知结点的后边
   *
   * @param existNode
   * @param newNode
   */
  def insertAfter(existNode: Node, newNode: Node): Unit = {
    existNode.next match {
      case None =>
        //如果已知结点为尾结点
        newNode.next = None
        existNode.next = Some(newNode)
      case Some(next) =>
        newNode.next = Some(next)
        existNode.next = Some(newNode)
    }
  }

  def insertBefore(existNode: Node, value: Int): Unit = {
    val newNode = new Node(value, None)
    insertBefore(existNode, newNode)
  }

  /**
   * 将新结点插入已知结点的前边
   * 需要校验链表中是否有环
   *
   * @param existNode
   * @param newNode
   */
  def insertBefore(existNode: Node, newNode: Node): Unit = {
    headOpt match {
      case None =>
        throw new IllegalStateException("head node should not be None")
      case Some(head) =>
        //如果已知结点为头结点
        if (existNode.equals(head)) {
          insertToHead(newNode)
        }

        var preNode = head
        while (preNode.next.isDefined && !preNode.next.get.equals(existNode)) {
          preNode = preNode.next.get
        }

        //已知结点在链表中不存在
        if (preNode.next.isEmpty) {
          throw new IllegalArgumentException("existNode " + existNode + " does not exist in this list")
        }
        newNode.next = preNode.next
        preNode.next = Some(newNode)
    }
  }

  /**
   * 将新结点插入到头部
   *
   * @param newNode
   */
  def insertToHead(newNode: Node): Unit = {
    headOpt match {
      case None =>
        headOpt = Some(newNode)
      case Some(head) =>
        newNode.next = Some(head)
        headOpt = Some(newNode)
    }
  }

  /**
   * 删除已知结点
   * 需要校验链表中是否有环
   *
   * @param node
   */
  def deleteByNode(node: Node): Unit = {
    //结点为头部时，无前结点，直接赋值为下一个结点
    if (headOpt.equals(node)) {
      headOpt = node.next
    } else {
      var preNode = headOpt.get
      while (preNode.next.isDefined && !preNode.next.get.equals(node)) {
        preNode = preNode.next.get
      }

      if (preNode.next.isEmpty) {
        throw new IllegalArgumentException("could not find given node")
      }
      preNode.next = node.next
    }
  }

  /**
   * 通过结点的值，找到链表中值的结点
   * 需要校验链表中是否有环
   *
   * @param value
   * @return
   */
  def findByValue(value: Int): Option[Node] = {
    var node = headOpt
    while (true) {
      node match {
        case Some(node) =>
          //找到值value的结点就返回结点
          if (node.data.equals(value)) return Some(node)
        //如果没有找到值就返回Node
        case None => return None
      }
      node = node.get.next
    }
    None
  }

  /**
   * 从指定结点反转链表方向到头结点
   *
   * @param node
   * @return
   */
  def inverseLink(node: Node): Node = {
    require(headOpt.isDefined, "the linked list is empty")
    var pre: Option[Node] = None
    var next: Option[Node] = None
    var current = headOpt

    //current 考虑链表尾结点时，next为空
    while (current.isDefined && !current.get.equals(node)) {
      //结点方向反转
      next = current.get.next
      current.get.next = pre
      //链表结点从头结点往尾结点方向移动
      pre = current
      current = next
    }
    //结点为指定结点时（即当前结点为新链表的头结点）方向反转
    current.get.next = pre
    current.get
  }

  /**
   * 判断是否为回文链表
   *
   * @return
   */
  def isPalindrome(): Boolean = {
    headOpt match {
      case None => false
      case Some(head) =>
        var pre: Option[Node] = None
        var next: Option[Node] = None
        var slow = head
        var fast = head

        if (slow.next.isEmpty) {
          return true
        }

        while (fast.next.isDefined && fast.next.get.next.isDefined) {
          fast = fast.next.get.next.get

          //反转链表方向
          next = slow.next
          slow.next = pre

          //移动慢结点到下一个结点
          pre = Some(slow)
          slow = next.get
        }

        var leftLink: Option[Node] = None
        var rightLink: Option[Node] = None
        //对齐比较的结点
        rightLink = slow.next
        //最后反转中点的方向
        slow.next = pre

        fast.next match {
          case None => //odd
            leftLink = pre
          case Some(_) => //even
            leftLink = Some(slow)
        }
        compareLinkedNodes(leftLink, rightLink)
    }
  }

  /**
   * 比较左右两个链表是否相等
   *
   * @param leftLink  左链表
   * @param rightLink 右链表
   * @return true-相等；false-不相等
   */
  def compareLinkedNodes(leftLink: Option[Node], rightLink: Option[Node]): Boolean = {
    var left = leftLink
    var right = rightLink

    breakable {
      while (left.isDefined && right.isDefined) {
        if (!left.get.data.equals(right.get.data)) {
          break()
        }
        left = left.get.next
        right = right.get.next
      }
    }
    left.isEmpty && right.isEmpty
  }

  def mkString(): String = {
    headOpt.map(head => {
      var node = head
      var result = new StringBuilder

      while (node.next.isDefined) {
        result.append(node.data)
        node = node.next.get
      }
      result.append(node.data)
      result.mkString
    }).getOrElse("")
  }

}
