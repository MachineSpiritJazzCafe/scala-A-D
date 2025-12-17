package datastructures

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random


class LinkedListSpec extends AnyFlatSpec with Matchers {
  
  val exampleArray = Array(1,2,3,4,5)

  "constructor" should "be able to construct a linked list" in {
    val emptyList = LinkedList.empty()

    emptyList.head shouldBe None
    emptyList.tail shouldBe None
    emptyList.length shouldBe 0
  }

  it should "be able to create a list from Array" in {
    val listFromArray = LinkedList.fromArray(exampleArray)

    listFromArray.length shouldBe exampleArray.length
    
    LinkedList.toArray(listFromArray).sameElements(exampleArray)
  }

  "append" should "add element to empty list" in {
    val testList = LinkedList.empty();
    
    testList.append(1)

    testList.length shouldBe 1
    testList.head.get.value shouldBe 1
    testList.tail.get.value shouldBe 1
  }
  
  it should "add element at the tail and increase lengthh" in {
    val testList = LinkedList.fromArray(exampleArray)

    testList.length shouldBe exampleArray.length
    
    testList.append(6)
    testList.tail.get.value shouldBe 6
    testList.length shouldBe 6
  }

  "pop" should "handle empty list" in {
    val testList = LinkedList.empty()

    testList.pop() shouldBe None
  }

  it should "leave empty list when removing last element" in {
    val testList = LinkedList.empty()

    testList.append(1)

    testList.length shouldBe 1
    testList.head.get.value shouldBe 1
    testList.tail.get.value shouldBe 1

    testList.pop().get.value shouldBe 1
    
    
    testList.print()

    testList.length shouldBe 0
    testList.head shouldBe None 
    testList.tail shouldBe None

  }

  it should "remove last element from list with multiple elements" in {
    val testList = LinkedList.fromArray(exampleArray)

    testList.pop().get.value shouldBe 5

    testList.length shouldBe exampleArray.length - 1
    testList.tail.get.value shouldBe 4
  }

  "prepend" should "add element to an empty list" in {
    val testList = LinkedList.empty()

    testList.prepend(1)

    testList.length shouldBe 1
    testList.head.get.value shouldBe 1 
    testList.tail.get.value shouldBe 1
  }

  it should "prepend to non empty list" in {
    val testList = LinkedList.fromArray(exampleArray)

    testList.prepend(0)

    testList.length shouldBe 6
    testList.head.get.value shouldBe 0
    testList.tail.get.value shouldBe 5
  }

  "popFirst" should "return None for empty list" in {
    val testList = LinkedList.empty()

    val popped = testList.popFirst()
    popped shouldBe None
    testList.length shouldBe 0
  }

  it should "remove one and only element from list" in {
    val testList = LinkedList.empty()

    testList.append(1)
    testList.length shouldBe 1
    testList.head.get.value shouldBe 1
    testList.tail.get.value shouldBe 1

    testList.popFirst()

    testList.length shouldBe 0
    testList.head shouldBe None
    testList.tail shouldBe None
  }

  it should "remove first element from the list and reset the head" in {
    val testList = LinkedList.fromArray(exampleArray)

    testList.popFirst()

    LinkedList.toArray(testList).sameElements(List(2,3,4,5))
    testList.length shouldBe 4
    testList.head.get.value shouldBe 2
  }

  "get" should "return None from empty list" in {
    val testList = LinkedList.empty()

    testList.get(7) shouldBe None
  }
  
  it should "get first element" in {
   val testList = LinkedList.fromArray(Array(1))
 
   testList.get(0).get.value shouldBe 1
   testList.length shouldBe 1
  }
  
  it should "get last element" in {
    val testList = LinkedList.fromArray(Array(1,2,3))

    val lastEle = testList.get(2)
    lastEle.get.value shouldBe 3
  }
  
  it should "return None if index outside of bounds" in {
   val testList = LinkedList.fromArray(Array(1,2,3)) 
   testList.get(5) shouldBe None 
  }
  
  "set" should "update value under index if found and return true" in {
    val testList = LinkedList.fromArray(Array(1,2))
    
    testList.set(1, 22) shouldBe true
    testList.tail.get.value shouldBe 22
  }

  it should "return false if not found" in {
   val testList = LinkedList.fromArray(exampleArray)
   
   testList.set(7, 10) shouldBe false
   LinkedList.toArray(testList).sameElements(exampleArray)
  }

  "insert" should "be able to prepend" in {
    val testList = LinkedList.empty()

    testList.insert(0, 0) shouldBe true
    testList.length shouldBe 1
    testList.head.get.value shouldBe 0
    testList.tail.get.value shouldBe 0
    
  }

  it should "be abble to append" in {
    val testList = LinkedList.fromArray(Array(1))

    testList.insert(1, 2) shouldBe true
    testList.length shouldBe 2
    testList.head.get.value shouldBe 1
    testList.tail.get.value shouldBe 2
  }

  it should "be able to insert somewhere in the middle" in {
    val testList = LinkedList.fromArray(Array(1,2,4,5))

    testList.insert(2, 3)
    testList.length shouldBe 5
    LinkedList.toArray(testList).sameElements(Array(1,2,3,4,5))
    testList.get(2).get.value shouldBe 3
  }

  "remove" should "return none from empty list" in {
    val testList = LinkedList.empty()

    testList.remove(3) shouldBe None
  }
  it should "remove and return first element" in {
    val testList = LinkedList.fromArray(Array(1,2,3,4))
    
    testList.remove(0).get.value shouldBe 1
    testList.length shouldBe 3
    LinkedList.toArray(testList).sameElements(Array(2,3,4))

  }

  it should "remove and return last element" in {
    val testList = LinkedList.fromArray(Array(1,2,3,4))
    
    testList.remove(3).get.value shouldBe 4
    testList.length shouldBe 3
    LinkedList.toArray(testList).sameElements(Array(1, 2, 3))
  }
  
  it should "remove element from the middle" in {
    val testList = LinkedList.fromArray(Array(1,2,3,4))
    
    testList.remove(2).get.value shouldBe 3
    testList.length shouldBe 3
    LinkedList.toArray(testList).sameElements(Array(1, 2, 4))
  }
  
  "reverse" should "return return empty list" in {
    val testList = LinkedList.empty()

    testList.reverse()
    testList.length shouldBe 0
    testList.head shouldBe None
    testList.tail shouldBe None
  }

  it should "reverse non empty list" in {
    var testList = LinkedList.fromArray(exampleArray)
    
    testList.reverse()

    LinkedList.toArray(testList).sameElements(exampleArray)

    testList.head.get.value shouldBe 5
    testList.tail.get.value shouldBe 1
    
    testList.get(0).get.value shouldBe 5
    testList.get(1).get.value shouldBe 4
    testList.get(2).get.value shouldBe 3
    testList.get(3).get.value shouldBe 2
    testList.get(4).get.value shouldBe 1
  }
}



