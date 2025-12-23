package problems

import datastructures.DoublyLinkedList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DoublyLinkedListProblemsSpec extends AnyFlatSpec with Matchers {
  import DoublyLinkedListProblems._


  "palindromeChecker" should "return false for empty list" in {
    val list = DoublyLinkedList.empty()
    palindromeChecker(list) shouldBe false
  }

  it should "return true for single item list" in {
    val list = DoublyLinkedList.fromArray((Array(1)))
    palindromeChecker(list) shouldBe true
  }

  it should "return true for list: 1-2-3-2-1" in {
    val list = DoublyLinkedList.fromArray((Array(1,2,3,2,1)))
    palindromeChecker(list) shouldBe true
  }

  it should "return true for list 1-2-2-1" in {
    val list = DoublyLinkedList.fromArray((Array(1,2,2,1)))
    palindromeChecker(list) shouldBe true
  }
  
  it should "return false for list 1-2-3-1" in {
    val list = DoublyLinkedList.fromArray((Array(1,2,3,1)))
    palindromeChecker(list) shouldBe false
  }

  it should "return false for list 1-2" in {
    val list = DoublyLinkedList.fromArray((Array(1,2)))
    palindromeChecker(list) shouldBe false
  }

  it should "return true for list 1-1" in {
    val list = DoublyLinkedList.fromArray((Array(1,1)))
    palindromeChecker(list) shouldBe true
  }
}

