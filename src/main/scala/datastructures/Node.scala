package datastructures 

case class Node(
  var value: Int, 
  var next: Option[Node] = None
)

