package datastructures

import java.lang.runtime.TemplateRuntime

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

  def append(value: Int): Boolean = {
    val node = BiNode(value, None, tail)
    if (length < 1) head = Some(node)
    tail.foreach(_.next = Some(node))
    tail = Some(node)
    length += 1
    true
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

  def prepend(value: Int): Boolean = {
    val node = BiNode(value, head, None)
    head.foreach(_.prev = Some(node))
    head = Some(node)

    length += 1
    if (length == 1) tail = Some(node)
    true
  }

  def popFirst(): Option[BiNode] = {
    val temp = head
    head = temp.flatMap(_.next)
    head.foreach(_.prev = None)
    temp.foreach(_.next = None)
    if (temp.isDefined) length -= 1
    if (length == 0) tail = None
    temp
  }

   def get(index: Int): Option[BiNode] = {
    if (index < 0 || index >= length) return None

    var temp: Option[BiNode] = None
    
    if (index < (length / 2))
      temp = head
      (0 until index).foreach { _ =>
        temp = temp.flatMap(_.next)
      }
    else
      temp = tail
      ((length - 1) until index by -1).foreach { _ =>
        temp = temp.flatMap(_.prev)
      }
    temp      
  }


  def set(index: Int, value: Int): Boolean = get(index) match {
    case Some(node) => node.value = value; true
    case None => false
  }

  def insert(index: Int, value: Int): Boolean = index match {
    case 0 => prepend(value)
    case i if (i == length) => append(value)
    case i if (i < 0 || i > length) => false
    case _ =>
      var pre = get(index - 1)
      var next = pre.flatMap(_.next)
      val node = BiNode(value, next, pre)
      pre.foreach(_.next = Some(node))
      next.foreach(_.prev = Some(node))
      length += 1
      true
  }

  def remove(index: Int): Option[BiNode] = index match {
    case 0 => popFirst()
    case i if (i == length -1) => pop()
    case _ =>
      var temp = get(index)
      var prev = temp.flatMap(_.prev)
      var next = temp.flatMap(_.next)

      prev.foreach(_.next = next)
      next.foreach(_.prev = prev)

      temp.foreach{ t =>
        t.prev = None
        t.next = None
        length -= 1
      }
      temp
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

