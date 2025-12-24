package datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StackSpec extends AnyFlatSpec with Matchers {
  
  "Stack" should "create empty stack" in {
    val stack = Stack.empty()
    stack.top shouldBe None
    stack.height shouldBe 0
  }
  
  it should "push onto the stack" in {
    val stack = Stack.empty()
    stack.push(1) 
    stack.top.get.value shouldBe 1
    stack.height shouldBe 1
  }

  it should "push onto non empty stack" in {
    val stack = Stack.fromArray(Array(1,2))
    stack.push(3)
    Stack.toArray(stack).toSeq shouldBe Seq(3,2,1)
    stack.top.get.value shouldBe 3
    stack.height shouldBe 3
  } 
  
  it should "pop from the top" in {
    val stack = Stack.fromArray(Array(1))

    stack.pop().get.value shouldBe 1
    stack.height shouldBe 0
    stack.top shouldBe None
  }

  it should "push and pop following LIFO" in {
   val stack = Stack.empty()

   stack.push(1)
   stack.top.get.value shouldBe 1
   stack.push(2)
   stack.top.get.value shouldBe 2
   stack.push(3)
   stack.top.get.value shouldBe 3
   Stack.toArray(stack).toSeq shouldBe Seq(3,2,1)
   stack.pop()
   Stack.toArray(stack).toSeq shouldBe Seq(2,1)
   stack.top.get.value shouldBe 2
   stack.pop()
   Stack.toArray(stack).toSeq shouldBe Seq(1)
   stack.top.get.value shouldBe 1
   stack.pop()
   Stack.toArray(stack).toSeq shouldBe Seq()
   stack.top shouldBe None
  }
}

