package problems

import datastructures.{Stack, Node}

object StackProblems {

  /**
   * Reverse List Using Stack
   * Related: General stack application for reversing sequences
   * 
   * Reverse a list by pushing all elements onto a stack,
   * then popping them off (LIFO gives reverse order).
   * Example: List(1, 2, 3, 4) becomes List(4, 3, 2, 1)
   * 
   * Solution (Stack-based reversal):
   * Time Complexity: O(n)
   * Space Complexity: O(n)
   */
  def reverseList(list: Seq[Int]): Seq[Int] = {
    val stack = Stack.empty()
    list.foreach(stack.push(_))

    var result = Seq.empty[Int]
    while(stack.top.isDefined){
      stack.pop().foreach{node => 
        result = result :+ node.value
      }
    }
    result
  }
}
