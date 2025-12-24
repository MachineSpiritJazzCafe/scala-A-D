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

  "reverse" should "return return empty list" in {
    val dll = DoublyLinkedList.empty()

    DoublyLinkedListProblems.reverse(dll)
    dll.length shouldBe 0
    dll.head shouldBe None
    dll.tail shouldBe None
  }

  it should "reverse non empty list" in {
    var dll = DoublyLinkedList.fromArray(Array(1,2,3,4,5))
    
    DoublyLinkedListProblems.reverse(dll)
    DoublyLinkedList.toArray(dll).toSeq shouldBe Seq(5,4,3,2,1)
    dll.head.get.value shouldBe 5
    dll.tail.get.value shouldBe 1
  }

  "partitionList" should "return empty list" in {
    DoublyLinkedListProblems.partition(
      DoublyLinkedList.empty(), 4).length shouldBe 0
  }
 
  it should "return the same list for all element < and > value" in {
    val list1 = DoublyLinkedListProblems.partition(
      DoublyLinkedList.fromArray(Array(1,2,3,4)), 5)
    val list2 = DoublyLinkedListProblems.partition(
      DoublyLinkedList.fromArray(Array(1,2,3,4)), 0)

    DoublyLinkedList.toArray(list1).toSeq shouldBe Seq(1,2,3,4)
    list1.head.get.value shouldBe 1
    list1.tail.get.value shouldBe 4
   
    DoublyLinkedList.toArray(list2).toSeq shouldBe Seq(1,2,3,4)
    list2.head.get.value shouldBe 1
    list2.tail.get.value shouldBe 4
  }
  
  it should "partition at end" in {
    val list = DoublyLinkedListProblems.partition(
    DoublyLinkedList.fromArray(Array(1,2,3,4,5,0)), 5)

    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(1,2,3,4,0, 5)
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 5
  }

  it should "partition at start" in {
    val list = DoublyLinkedListProblems.partition(
    DoublyLinkedList.fromArray(Array(5,1,2,3,4)), 5)

    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(1,2,3,4,5)
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 5
  }
  
  it should "partition in middle" in {
    val list = DoublyLinkedListProblems.partition(
    DoublyLinkedList.fromArray(Array(1,2,7,6,5,4,8,9)), 6)

    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(1,2,5,4,7,6,8,9)
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 9
  }
}

