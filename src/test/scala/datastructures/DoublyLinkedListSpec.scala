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
}
