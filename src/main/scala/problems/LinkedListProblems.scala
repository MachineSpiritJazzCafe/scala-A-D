package problems

import datastructures.{LinkedList, Node}

object LinkedListProblems {
  /**
    * LeetCode 876: Middle of the Linked List
    * https://leetcode.com/problems/middle-of-the-linked-list/
    * 
    * Given the head of a singly linked list, return the middle node.
    * If there are two middle nodes, return the second middle node.
    * 
    * Time Complexity: O(n)
    * Space Complexity: O(1)
    */
  def findMiddleNode(list: LinkedList): Option[Node] = {
    if (list.head.isEmpty) return None

    var fast = list.head
    var slow = list.head

    while(fast.isDefined && fast.flatMap(_.next).isDefined)
      slow = slow.flatMap(_.next)
      fast = fast.flatMap(_.next.flatMap(_.next))
    slow  
  } 


  /**
   * LeetCode 141: Linked List Cycle
   * https://leetcode.com/problems/linked-list-cycle/
   * 
   * Given head of a linked list, determine if the linked list has a cycle in it.
   * A cycle exists if some node can be reached again by continuously following next.
   * Return true if there is a cycle, otherwise return false.
   * 
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def hasLoop(list: LinkedList): Boolean = {
    /** A two pointer solution */
    var slow = list.head
    var fast = list.head

    while (fast.isDefined && fast.flatMap(_.next).isDefined) {
      slow = slow.flatMap(_.next)
      fast = fast.flatMap(_.next.flatMap(_.next))

      if (slow.get.value == fast.get.value) return true
    }
    false
  }
}

