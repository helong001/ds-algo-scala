package ds_algo.queue

class ArrayQueueTest extends DemoQueueTest {

  override def getInstance() = new ArrayQueue[Int](15)
}
