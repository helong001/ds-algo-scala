package ds_algo.queue

class CircularQueueTest extends DemoQueueTest {

  override def getInstance(): DemoQueue[Int] = new CircularQueue[Int](15)
}
