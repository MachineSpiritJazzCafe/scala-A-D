package datastructures

case class BiNode(
  var value: Int,
  var next: Option[BiNode],
  var prev: Option[BiNode]
)

