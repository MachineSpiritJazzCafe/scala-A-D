package problems

import datastructures.{Stack, Node}

object QueueProblems {


  /**
 * Queue Using Two Stacks
 * Implement a FIFO queue using only two stacks (enqueue, dequeue, peek operations)
 * 
 * Use input stack for enqueue, output stack for dequeue. When output is empty,
 * transfer all elements from input to output (reversing order gives FIFO behavior).
 * Example: enqueue(1,2,3) then dequeue() → 1 (first in, first out)
 * 
 * Solution (Two-stack transfer):
 * Time Complexity: O(1) enqueue, O(1) amortized dequeue (O(n) worst case per dequeue)
 * Space Complexity: O(n) - two stacks store n total elements
 */
  class StacksQueue{
    val stack1: Stack = Stack.empty()
    val stack2: Stack = Stack.empty()
    
    def enqueue(value: Int) = {
      while (!stack1.isEmpty)
        stack1.pop().foreach(n => stack2.push(n.value))
      
      stack1.push(value)
      
      while (!stack2.isEmpty)
        stack2.pop().foreach(n => stack1.push(n.value))

    }

    def dequeue(): Option[Node] = stack1.pop()
    
    def peek(): Option[Int] = stack1.top.map(_.value)
    def isEmpty() = stack1.isEmpty 
  }

  object StacksQueue {
    def empty = new StacksQueue()
  }
}
