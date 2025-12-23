package datastructures

import datastructures.DoublyLinkedList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DoublyLinkedListSpec extends AnyFlatSpec with Matchers {

  "append" should "add element to the tail of the empty list" in {
    val list = DoublyLinkedList.empty()

    list.append(1)
    list.length shouldBe 1
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 1
  }

  it should "add elements at the end to non empty list" in {
    val list = DoublyLinkedList.fromArray(Array(1,2))

    list.append(3)

    list.length shouldBe 3
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 3
  }

  "pop" should "return None when popping from empty list" in {
    val list = DoublyLinkedList.empty()

    list.pop() shouldBe None
  }

  it should "return last element and leave list empty" in {
    val list = DoublyLinkedList.fromArray(Array(1))

    list.pop().get.value shouldBe 1

    list.length shouldBe 0 
    list.head shouldBe None 
    list.tail shouldBe None
  }

  it should "return last element from multi element list" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3))

    list.pop().get.value shouldBe 3

    list.length shouldBe 2
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 2

    list.head.get.next shouldBe list.tail
    list.tail.get.prev shouldBe list.head 
  }

  "prepend" should "add single element to empty list" in {
    val list = DoublyLinkedList.empty()

    list.prepend(1)

    list.head.get.value shouldBe 1
    list.head.get.next shouldBe None
    list.head.get.prev shouldBe None
    list.tail.get.value shouldBe 1
    list.tail.get.prev shouldBe None
    list.tail.get.next shouldBe None
    list.length shouldBe 1
  }
  
  it should "add element to non empty list" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3))

    list.prepend(0)

    list.head.get.value shouldBe 0
    list.head.get.prev shouldBe None
    list.head.get.next.get.value shouldBe 1
    list.tail.get.value shouldBe 3
  }

  "popFirst" should "return None from empty list" in {
    val list = DoublyLinkedList.empty()

    val popped = list.popFirst()

    popped shouldBe None
    list.length shouldBe 0
  }

  it should "return element and empty the list from remainig element" in {
    val list = DoublyLinkedList.fromArray(Array(1))

    val popped = list.popFirst()

    popped.get.value shouldBe 1
    list.head shouldBe None
    list.tail shouldBe None
    list.length shouldBe 0
  }

  it should "return first element from non empty list" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3,4))

    val popped = list.popFirst()
    
    popped.get.value shouldBe 1
    list.head.get.value shouldBe 2
    list.head.get.next.get.value shouldBe 3
    list.head.get.prev shouldBe None
    list.tail.get.value shouldBe 4
    list.length shouldBe 3
  }

  "get" should "return None from empty list" in {
    val list = DoublyLinkedList.empty()

    list.get(1) shouldBe None
  }

  it should "return node from 0 index" in {
    val list = DoublyLinkedList.fromArray(Array(1))

    list.get(0).get.value shouldBe 1
    list.head.get.value shouldBe 1
    list.tail.get.value shouldBe 1
  }

  it should "return node from last index" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3))

    list.get(2).get.value shouldBe 3
  }

  it should "return node from the middle" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3,4,5))
   
    list.get(2).get.value shouldBe 3
  }

  "set" should "return false for empty list" in {
    val list = DoublyLinkedList.empty()

    list.set(0, 1) shouldBe false
    list.length shouldBe 0
  }

  it should "return true and set value in head and tail" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3))

    list.set(0, 11)
    list.head.get.value shouldBe 11

    list.set(2, 33)
    list.tail.get.value shouldBe 33
    list.length shouldBe 3
  }

  it should "return true and set value in the middle" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3,4,5))

    list.set(2, 33) 
    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(1,2,33,4,5)
  }

  "insert" should "set a value under index 0 to an empty list" in {
    val list = DoublyLinkedList.empty()

    list.insert(0, 0)
    
    list.length shouldBe 1
    list.head.get.value shouldBe 0
    list.tail.get.value shouldBe 0
  }

  it should "return false if index is < 0 or > length" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3))

    list.insert(-1, 0) shouldBe false
    list.insert(4, 0) shouldBe false
    
    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(1,2,3)
  }

  it should "append value to a non empty list at the beginning" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3))

    list.insert(0, 0) shouldBe true 
    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(0,1,2,3)
  }

  it should "set a value to a non empty list at the end" in {
    
    val list = DoublyLinkedList.fromArray(Array(1,2,3))

    list.insert(3, 4) shouldBe true 
    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(1,2,3,4)
  }

  it should "set a value in the middle" in {
    
    val list = DoublyLinkedList.fromArray(Array(1,2,3))

    list.insert(2, 22) shouldBe true
    
    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(1,2,22,3)
  }

  "remove" should "return None from empty list" in {
    DoublyLinkedList.empty().remove(0) shouldBe None
  }

  it should "return head node and update list" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3,4))
    
    list.remove(0).get.value shouldBe 1
    list.length shouldBe 3
    list.head.get.value shouldBe 2
    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(2,3,4)
  }

  it should "return tail node and update list" in {
    val list = DoublyLinkedList.fromArray(Array(1,2,3,4))
    
    list.remove(3).get.value shouldBe 4
    list.remove(0).get.value shouldBe 1
    list.length shouldBe 2
    list.tail.get.value shouldBe 3
    list.head.get.value shouldBe 2
    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(2,3)
  }
  
  it should "return middle node and update list" in {
    
    val list = DoublyLinkedList.fromArray(Array(1,2,3,4))
    
    list.remove(2).get.value shouldBe 3
    list.length shouldBe 3
    DoublyLinkedList.toArray(list).toSeq shouldBe Seq(1,2,4)
  }
}
