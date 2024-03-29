package ds_algo.queue

trait DemoQueue[T] {
  
  var size = 0

  def enqueue(data: T): Unit

  def dequeue(): Option[T]
}
