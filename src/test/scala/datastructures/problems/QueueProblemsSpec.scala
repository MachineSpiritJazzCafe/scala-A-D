package problems

import datastructures.Stack
import QueueProblems._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QueueProblemsSpec extends AnyFlatSpec with Matchers {

  "Queue using stacks" should "allow to enqueue to empty list" in {
    val stack = StacksQueue.empty 
    stack.enqueue(1)

    stack.stack1.height shouldBe 1
  }

  it should "enqueue into non empty StacksQueue remaining queue order" in {
    val stack = StacksQueue.empty
    stack.enqueue(1)
    stack.enqueue(2)
    stack.enqueue(3)

    Stack.toArray(stack.stack1).toSeq sameElements Seq(3,2,1)
  }
  
  it should "safely dequeue from empty StacksQueue" in {
    StacksQueue.empty.dequeue() shouldBe None
  }
    
  it should "dequeue from non empty StacksQueue maintaining queue behavior" in {
    val stack = StacksQueue.empty 
    stack.enqueue(1)
    stack.enqueue(2)
    stack.enqueue(3)

    stack.dequeue().get.value shouldBe 1
    stack.dequeue().get.value shouldBe 2
    stack.dequeue().get.value shouldBe 3


  }

}
