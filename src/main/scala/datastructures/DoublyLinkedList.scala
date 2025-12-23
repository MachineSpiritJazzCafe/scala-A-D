package datastructures

class DoublyLinkedList{
  var length: Int = 0
  var head: Option[BiNode] = None
  var tail: Option[BiNode] = None

  def print() = {
    var temp = head
    while(temp.isDefined)
      println(temp.get.value)
      temp = temp.flatMap(_.next)
  }

  def append(value: Int): Unit = {
    val node = BiNode(value, None, tail)
    if (length < 1) head = Some(node)
    tail.foreach(_.next = Some(node))
    tail = Some(node)
    length += 1
  }

  def pop(): Option[BiNode] = {
    var prev = tail.flatMap(_.prev)
    prev.foreach(_.next = None)
    var popped = tail
    tail = prev
    popped.foreach(_.next = None)
    length -= 1
    if (length == 0) head = None 
    popped
  }
}

object DoublyLinkedList {
  
  def fromArray(arr: Array[Int]): DoublyLinkedList = {
    val list = new DoublyLinkedList()
    arr.foreach(list.append)
    list
  }

  def toArray(ll: DoublyLinkedList): Array[Int] = {
    var arr = Array.empty[Int]
    var current = ll.head
    if (ll.length < 0) arr
    else
      while (current.isDefined) {
        arr = arr :+ current.get.value
        current = current.get.next
      }
    arr  
  }

  def empty(): DoublyLinkedList = new DoublyLinkedList()
}

