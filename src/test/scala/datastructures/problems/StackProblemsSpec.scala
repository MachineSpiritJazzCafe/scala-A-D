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
    StackProblems.reverseList(Seq(1,2,3,4)) shouldBe Seq(4,3,2,1)
  }
  

}

