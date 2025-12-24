package datastructures

class Stack {
  var height: Int = 0
  var top: Option[Node] = None

  def push(value: Int) = {
    val node = Node(value, top)
    top = Some(node)
    height += 1
  }

  def pop(): Option[Node] = {
    var toPop = top
    top = toPop.flatMap(_.next)
    toPop.foreach(_.next = None)
    if (toPop.isDefined) height -= 1
    toPop
  }

  def print(): Unit = {
    var temp = top
    while(temp.isDefined)
      println(temp.get.value)
      temp = temp.flatMap(_.next)  
  }
}

object Stack {

  def fromArray(arr: Array[Int]): Stack = {
    val stack = new Stack()
    arr.foreach(stack.push(_))
    stack
  }

  def toArray(stack: Stack): Array[Int] = {
    var arr = Array.empty[Int]
    var current = stack.top
    if (stack.height < 0) arr
    else
      while (current.isDefined) {
        arr = arr :+ current.get.value
        current = current.get.next
      }
    arr  
  }

  def empty(): Stack = new Stack()
}
