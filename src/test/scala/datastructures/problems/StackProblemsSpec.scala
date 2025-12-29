package problems

import datastructures.Stack
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StackProblemsSpec extends AnyFlatSpec with Matchers {
  
  "ReverseList" should "return empty list if empty" in {
    StackProblems.reverseList(Seq()) shouldBe Seq()
  }
  
  it should "return list with just one element" in {
    StackProblems.reverseList(Seq(1)) shouldBe Seq(1)
  }

  it should "reverse Seq(1,2,3,4) into Seq(4,3,2,1)" in {
    StackProblems
      .reverseList(Seq(1,2,3,4)) shouldBe Seq(4,3,2,1)
  }
  
  "ReverseListF" should "return empty list if empty" in {
    StackProblems.reverseListF(Seq()) shouldBe Seq()
  }
  
  it should "return list with just one element" in {
    StackProblems
      .reverseListF(Seq(1)) shouldBe Seq(1)
  }

  it should "reverse Seq(1,2,3,4) into Seq(4,3,2,1)" in {
    StackProblems
      .reverseListF(Seq(1,2,3,4)) shouldBe Seq(4,3,2,1)
  }

  "IsBalanced" should "return true for empty list" in {
    StackProblems.isBalanced(Seq()) shouldBe true
  }

  it should "return false for non even lits" in {
    StackProblems
      .isBalanced(Seq(111)) shouldBe false
  }

  it should "return true for Seq(1,2,3,3,2,1)" in {
    StackProblems
      .isBalanced(Seq(1,2,3,3,2,1)) shouldBe true
  }

  "Reverse" should "return empty stack" in {
    StackProblems.sort(Stack.empty()).isEmpty shouldBe true
  }
  
  it should "return single element without change" in {
    val res = StackProblems.sort(Stack.fromArray(Array(1)))
    
    res.height shouldBe 1
    res.top.get.value shouldBe 1
  }
      
  it should "return storted stack" in {
    val res = StackProblems.sort(Stack.fromArray(Array(3,2,1)))
    Stack.toArray(res).toSeq sameElements Seq(3,2,1)
  }

  it should "sort stack with 2 elements" in {
    val res = StackProblems.sort(Stack.fromArray(Array(1,2)))
    Stack.toArray(res).toSeq sameElements Seq(2,1)
  }

  it should "sort non empty stack" in {
    val res = StackProblems.sort(Stack.fromArray(Array(1,5,3,2,5)))
    Stack.toArray(res).toSeq sameElements Seq(6,5,3,2,1)
  } 
}

