package ds_algo.stack

/**
 * 浏览器的前进和后退功能demo
 *
 * @param currentPageOpt 当前页面
 * @param backStack      后退栈
 * @param forwardStack   前进栈
 */
class BrowserDemo(var currentPageOpt: Option[String], val backStack: StackBaseLinkedList[String],
                  val forwardStack: StackBaseLinkedList[String]) {
  def this() = this(None, new StackBaseLinkedList[String], new StackBaseLinkedList[String])

  /**
   * 打开新的网页
   * 1）将当前网页压入后退栈中
   * 2）前进栈失效-清空
   * 3）新的网页为当前页面
   *
   * @param newPage
   */
  def open(newPage: String): Unit = {
    currentPageOpt.foreach(backStack.push)
    forwardStack.clear()
    currentPageOpt = Some(newPage)
  }

  /**
   * 判断浏览器是否还能后退
   *
   * @return
   */
  def canGoBack(): Boolean = backStack.size > 0

  /**
   * 浏览器后退
   * 1）将当前页面压入前进栈中
   * 2）从后退栈中取出新的页面作为当前页面
   */
  def goBack(): Unit = {
    forwardStack.push(currentPageOpt.get)
    currentPageOpt = Some(backStack.pop().get.data)
  }

  /**
   * 判断浏览器是否还能前进
   *
   * @return
   */
  def canGoForward(): Boolean = forwardStack.size > 0

  /**
   * 浏览器前进
   * 1）将当前页面压入后退栈中
   * 2）从前进栈中取出新的页面作为当前页面
   */
  def goForward(): Unit = {
    backStack.push(currentPageOpt.get)
    currentPageOpt = Some(forwardStack.pop().get.data)
  }
}
