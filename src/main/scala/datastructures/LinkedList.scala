package datastrcuctures

class LinkedList {
  var head: Option[Node] = None
  var tail: Option[Node] = None
  var length: Int = 0


  def print(): Unit = {
    var current = head
    while (current.isDefined) {
      println(current.get.value)
      current = current.get.next
    }
  }
  
  def append(value: Int): Boolean = {
    var newNode = Some(Node(value))
    
    if (tail.isEmpty) head = newNode
    else tail.get.next = newNode

    tail = newNode 
    length += 1
    true
  }

  def pop(): Option[Node] = {
    var popped = head
    if (length > 1) {
      var pre = head
      var temp = head
      while(temp.get.next.isDefined) {
        pre = temp
        temp = temp.get.next
      }
      tail = pre
      tail.map(_.next = None)
      popped = temp
    }
    length -= 1
    if (length == 0) 
      head = None
      tail = None
    popped
  }


  def prepend(value: Int): Boolean = {
    var newNode = Some(Node(value, head))
    head = newNode
    if (length == 0) tail = newNode
    length += 1
    true
  }

  def popFirst(): Option[Node] = {
    val next = head.flatMap(_.next)
    val popped = head match {
      case Some(n) => { 
        n.next = None
        length -= 1
        if (length == 0)
          head = None
          tail = None
        Some(n) 
      }
      case None => head
    }
    head = next
    popped
  }

  def get(index: Int): Option[Node] = {
    if (index < 0 || index >= length) None
    
    var temp = head
    for (_ <- 0 until index) {
      temp = temp.flatMap(_.next)
    }
    temp
  }

  def set(index: Int, value: Int): Boolean = {
    var node = get(index)
    node.map(_.value = value).isDefined
  }

  def insert(index: Int, value: Int): Boolean = 
    index match {
      case i if i < 0 || i > length => false
      case 0 => prepend(value)
      case i if i == length => append(value)
      case _ => 
        var newNode = Node(value)
        var pre = get(index - 1)
        newNode.next = pre.flatMap(_.next)
        pre.map(_.next = Some(newNode))
        length += 1
        true
    }

  def remove(index: Int): Option[Node] = index match {
    case i if (i < 0 || i > length) => None
    case 0 => popFirst()
    case i if i == length => pop()
    case _ =>
      val pre = get(index - 1)
      val toRemove = pre.flatMap(_.next)
      pre.map(_.next = toRemove.flatMap(_.next))
      toRemove.map(_.next = None)
      length -= 1
      toRemove
  }

  def reverse(): Unit = {
    var temp = head
    head = tail
    tail = head
    var before: Option[Node] = None
    var after = temp
    for (_ <- 0 until length) 
      after = temp.flatMap(_.next)
      temp.map(_.next = before)
      before = temp
      temp = after
  }
}


object LinkedList {
  
  def fromArray(arr: Array[Int]): LinkedList = {
    val list = new LinkedList()
    arr.foreach(list.append)
    list
  }

  def toArray(ll: LinkedList): Array[Int] = {
    var arr = Array.empty[Int]
    var current = ll.head
    if (ll.length < 0) arr
    else
      while (current.isDefined) {
        arr :+ current.get.value
        current = current.get.next
      }
    arr  
  }

  def empty(): LinkedList = new LinkedList()
}
