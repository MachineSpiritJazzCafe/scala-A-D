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

  /** Another version using Iterator */
  def reverseListF(list: Seq[Int]): Seq[Int] = {
  val stack = Stack.empty()
  list.foreach(stack.push(_))
  
  Iterator.continually(stack.pop())
    .takeWhile(_.isDefined)
    .flatten
    .map(_.value)
    .toSeq
  }

  /**
 * Is Balanced (Numbers)
 * Check if a list of numbers is balanced (mirror/palindrome check using stack)
 * 
 * Split list in half, push first half onto stack, then compare with second half.
 * Example: List(1, 2, 3, 3, 2, 1) is balanced, List(1, 2, 3, 4) is not
 * 
 * Solution (Stack-based):
 * Time Complexity: O(n)
 * Space Complexity: O(n/2) = O(n)
 */
  def isBalanced(list: Seq[Int]): Boolean = {
    if (list.length %2 != 0) false
    else
      var stack = Stack.empty()
      val (first, second) = list.splitAt(list.length / 2)

      first.foreach(stack.push(_))
      
      second.forall{ sec =>
        stack.pop().map(_.value == sec)
          .exists(identity)
      }
  }
}
