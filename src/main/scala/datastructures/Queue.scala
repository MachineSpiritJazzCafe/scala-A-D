package datastructures

class Queue {
  var lenght: Int = 0
  var first: Option[Node] = None
  var last: Option[Node] = None

  def enqueue(value: Int) = {
    val node: Option[Node] = Some(Node(value))
    last.foreach(_.next = node)
    if (first.isEmpty) first = node
    last = node
    lenght += 1
  }

  def dequeue(): Option[Node] = {
    var temp = first.flatMap(_.next)
    var deqNode = first
    deqNode.foreach(_.next = None)
    first = temp
    deqNode.foreach(_ => lenght -= 1)
    if (first.isEmpty) last = None
    deqNode
  }

  def print(): Unit = {
    var temp = first
    while(temp.isDefined)
      println(temp.get.value)
      temp = temp.flatMap(_.next)  
  }
}

object Queue {

  def fromArray(arr: Array[Int]): Queue = {
    val queue = new Queue()
    arr.foreach(queue.enqueue(_))
    queue
  }

  def toArray(queue: Queue): Array[Int] = {
    var arr = Array.empty[Int]
    var current = queue.first
    if (queue.lenght < 0) arr
    else
      while (current.isDefined) {
        arr = arr :+ current.get.value
        current = current.get.next
      }
    arr  
  }

  def empty(): Queue = new Queue()
}

