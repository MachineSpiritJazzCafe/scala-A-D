package problems

import datastructures.LinkedList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkedListProblemsSpec extends AnyFlatSpec with Matchers {

  "findMiddleNode" should "return None for empty list" in {
    val list = LinkedList.empty()
    
    LinkedListProblems.findMiddleNode(list) shouldBe None
  }
  
  it should "return the only node for single-element list" in {
    val list = LinkedList.fromArray(Array(42))
    
    val middle = LinkedListProblems.findMiddleNode(list)
    middle.get.value shouldBe 42
  }
  
  it should "return node with a value 3 from a 5 ele array" in {
  val list = LinkedList.fromArray(Array(1,2,3,4,5))

  val middle = LinkedListProblems.findMiddleNode(list)
  middle.get.value shouldBe 3
  }

  it should "return second middle element in even-length lists" in {
    val list = LinkedList.fromArray(Array(1,2,3,4,5,6))
    val middle = LinkedListProblems.findMiddleNode(list)
    middle.get.value shouldBe 4
  }

  "HasLoop" should "return false for empty list" in {
    val list = LinkedList.empty()

    LinkedListProblems.hasLoop(list) shouldBe false
  }

  it should "return false if list has no loop" in {
    val list = LinkedList.fromArray(Array(1,2,3,4,5))

    LinkedListProblems.hasLoop(list) shouldBe false
  }
  
  it should "return true if loop is present in the list" in {
    val list = LinkedList.fromArray(Array(1,2,3,4,5))

    list.tail.get.next = list.head
    
    LinkedListProblems.hasLoop(list) shouldBe true
  }

  "findKthNode" should "return None from empty list" in {
    val list = LinkedList.empty()

    LinkedListProblems.findKthNode(list, 0) shouldBe None
  }

  it should "return None if k larger than list" in {
    val list = LinkedList.fromArray(Array(1))

    LinkedListProblems.findKthNode(list, 4) shouldBe None 
  }

  it should "return the kth element from end" in {
    val list = LinkedList.fromArray(Array(1,2,3,4,5,6,7,8,9,10))

    LinkedListProblems.findKthNode(list, 3).get.value shouldBe 8
  }

  "removeDuplicatesWithSet" should "return empty list such passed" in {
    LinkedListProblems.removeDuplicatesWithSet(
      LinkedList.empty()).length shouldBe 0 
  }

  it should "return unchanged list if no duplicates found" in {
    LinkedListProblems.removeDuplicatesWithSet(
      LinkedList.fromArray(Array(1,2,3))).length shouldBe 3
  }

  it should "remove one extra element" in {
    LinkedList.toArray(LinkedListProblems.removeDuplicatesWithSet(
      LinkedList.fromArray(Array(1,2,3, 1))))
        .sameElements(Array(1,2,3))
  }
 
  it should "remove all extra elements" in {
    LinkedList.toArray(LinkedListProblems.removeDuplicatesWithSet(
      LinkedList.fromArray(Array(1,2,3,1,3,3,1,2,2,4))))
        .sameElements(Array(1,2,3,4))
  }


  "removeDuplicatesInNsq" should "return empty list such passed" in {
    LinkedListProblems.removeDuplicatesInNSq(
      LinkedList.empty()).length shouldBe 0
  }

  it should "return unchanged list if no duplicates found" in {
    LinkedListProblems.removeDuplicatesInNSq(
      LinkedList.fromArray(Array(1,2,3))).length shouldBe 3
  }

  it should "remove one extra element" in {
    LinkedList.toArray(LinkedListProblems.removeDuplicatesInNSq(
      LinkedList.fromArray(Array(1,2,3, 1)))).sameElements(Array(1,2,3))
  }
  
  it should "remove all extra elements" in {
    LinkedList.toArray(LinkedListProblems.removeDuplicatesWithSet(
      LinkedList.fromArray(Array(1,2,3,1,3,3,1,2,2,4))))
        .sameElements(Array(1,2,3,4))
  }

  "binaryToDecimal" should "return None for empty list" in {
    LinkedListProblems.binaryToDecimal(
      LinkedList.empty()) shouldBe None
  }

  it should "return 0 for 0" in {
    LinkedListProblems.binaryToDecimal(
      LinkedList.fromArray(Array(0))) shouldBe Some(0)
  }

  it should "return 1 for 1" in {
    LinkedListProblems.binaryToDecimal(
      LinkedList.fromArray(Array(1))) shouldBe Some(1)
  }

  it should "return 8 for 1000" in {
    LinkedListProblems.binaryToDecimal(
      LinkedList.fromArray(Array(1,0,0,0))) shouldBe Some(8)
  }
  
  it should "return 13 for 1101" in {
    LinkedListProblems.binaryToDecimal(
      LinkedList.fromArray(Array(1,1,0,1))) shouldBe Some(13)
  }
  
  it should "return 6 for 110" in {
    LinkedListProblems.binaryToDecimal(
      LinkedList.fromArray(Array(1,1,0))) shouldBe Some(6)
  }

  "partitionList" should "return empty list" in {
    LinkedListProblems.partitionList(
      LinkedList.empty(), 4).length shouldBe 0
  }
 
  it should "return the same list for all element < and > value" in {
    val list1 = LinkedListProblems.partitionList(
      LinkedList.fromArray(Array(1,2,3,4)), 5)
    val list2 = LinkedListProblems.partitionList(
      LinkedList.fromArray(Array(1,2,3,4)), 0)

    LinkedList.toArray(list1).toSeq shouldBe Seq(1,2,3,4)
    LinkedList.toArray(list2).toSeq shouldBe Seq(1,2,3,4)
  }
  
  it should "partition at end" in {
    val list = LinkedListProblems.partitionList(
      LinkedList.fromArray(Array(1,2,3,4,5,0)), 5)

    LinkedList.toArray(list).toSeq shouldBe Seq(1,2,3,4,0, 5)
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 5
  }

  it should "partition at start" in {
   val list = LinkedListProblems.partitionList(
      LinkedList.fromArray(Array(5,1,2,3,4)), 5)

    LinkedList.toArray(list).toSeq shouldBe Seq(1,2,3,4,5)
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 5
  }
  
  it should "partition in middle" in {
    val list = LinkedListProblems.partitionList(
      LinkedList.fromArray(Array(1,2,7,6,5,4,8,9)), 6)

    LinkedList.toArray(list).toSeq shouldBe Seq(1,2,5,4,7,6,8,9)
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 9
  }
} 
