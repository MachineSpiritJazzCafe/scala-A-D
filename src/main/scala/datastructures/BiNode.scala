package datastructures

case class BiNode(
  var value: Int,
  var next: Option[BiNode] = None,
  var prev: Option[BiNode] = None
)

