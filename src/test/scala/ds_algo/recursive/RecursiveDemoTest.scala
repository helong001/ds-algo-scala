package ds_algo.recursive

import org.scalatest.{FlatSpec, Matchers}

class RecursiveDemoTest extends FlatSpec with Matchers {

  behavior of "RecursiveDemoTest"

  it should "calculateStepWays" in {
    RecursiveDemo.calculateStepWays(1) should equal(1)
    RecursiveDemo.calculateStepWays(2) should equal(2)
    RecursiveDemo.calculateStepWays(3) should equal(3)
    RecursiveDemo.calculateStepWays(4) should equal(5)
    RecursiveDemo.calculateStepWays(40) should equal(165580141)
  }

  it should "fibonacciRecursive" in {
    RecursiveDemo.fibonacciRecursive(1) should equal(1)
    RecursiveDemo.fibonacciRecursive(2) should equal(1)
    RecursiveDemo.fibonacciRecursive(3) should equal(2)
    RecursiveDemo.fibonacciRecursive(4) should equal(3)
    RecursiveDemo.fibonacciRecursive(5) should equal(5)
    RecursiveDemo.fibonacciRecursive(6) should equal(8)
    RecursiveDemo.fibonacciRecursive(70) should equal(885444751)
  }

  it should "fibonacciTailRecursive" in {
    RecursiveDemo.fibonacciTailRecursive(1, 1, 1) should equal(1)
    RecursiveDemo.fibonacciTailRecursive(2, 1, 1) should equal(1)
    RecursiveDemo.fibonacciTailRecursive(3, 1, 1) should equal(2)
    RecursiveDemo.fibonacciTailRecursive(4, 1, 1) should equal(3)
    RecursiveDemo.fibonacciTailRecursive(5, 1, 1) should equal(5)
    RecursiveDemo.fibonacciTailRecursive(6, 1, 1) should equal(8)
    RecursiveDemo.fibonacciRecursive(70) should equal(885444751)
  }

}
