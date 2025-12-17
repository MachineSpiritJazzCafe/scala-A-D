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
}
