package ds_algo.linkedlist

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

  def insertHead(value: Int): Unit = {
    val newNode = new Node(value, None)
    insertHead(newNode)
  }

  /**
   * 将新结点插入到头部
   *
   * @param newNode
   */
  def insertHead(newNode: Node): Unit = {
    headOpt match {
      case None =>
        headOpt = Some(newNode)
      case Some(head) =>
        newNode.next = Some(head)
        headOpt = Some(newNode)
    }
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
          insertHead(newNode)
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

  def inverseLink(node: Node): Node = {

    None.get
  }

}
