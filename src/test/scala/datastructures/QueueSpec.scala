package datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.channels.NonWritableChannelException

class QueueSpec extends AnyFlatSpec with Matchers {
  
  "Queue" should "create empty queue" in {
    val queue = Queue.empty()
    queue.first shouldBe None
    queue.last shouldBe None
    queue.lenght shouldBe 0
  }
  
  it should "enqueue onto the queue" in {
    val queue = Queue.empty()
    queue.enqueue(1)
    queue.first.get.value shouldBe 1
    queue.last.get.value shouldBe 1
    queue.lenght shouldBe 1
  }

  it should "enqueue onto non empty queue" in {
    val queue = Queue.fromArray(Array(1,2))
    queue.enqueue(3)
    Queue.toArray(queue).toSeq shouldBe Seq(1,2,3)
    queue.first.get.value shouldBe 1
    queue.last.get.value shouldBe 3
    queue.lenght shouldBe 3
  } 
  
  it should "dequeue from the first" in {
    val queue = Queue.fromArray(Array(1))

    queue.dequeue().get.value shouldBe 1
    queue.lenght shouldBe 0
    queue.first shouldBe None
  }

  it should "enqueue and dequeue following FIFO" in {
   val queue = Queue.empty()

   queue.enqueue(1)
   queue.first.get.value shouldBe 1
   queue.last.get.value shouldBe 1
   queue.enqueue(2)
   queue.first.get.value shouldBe 1
   queue.last.get.value shouldBe 2
   queue.enqueue(3)
   queue.first.get.value shouldBe 1
   queue.last.get.value shouldBe 3
   Queue.toArray(queue).toSeq shouldBe Seq(1,2,3)
   queue.dequeue()
   Queue.toArray(queue).toSeq shouldBe Seq(2,3)
   queue.first.get.value shouldBe 2
   queue.dequeue()
   Queue.toArray(queue).toSeq shouldBe Seq(3)
   queue.first.get.value shouldBe 3
   queue.dequeue()
   Queue.toArray(queue).toSeq shouldBe Seq()
   queue.first shouldBe None
   queue.last shouldBe None 
  }
}
