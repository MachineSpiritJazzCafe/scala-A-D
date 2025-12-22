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


    /**
   * Find Kth Node From End
   * Related: LeetCode 19: Remove Nth Node From End of List
   * https://leetcode.com/problems/remove-nth-node-from-end-of-list/
   * 
   * Given the head of a linked list, return the kth node from the end.
   * If k is greater than the length of the list, return None.
   * k is 1-indexed (k=1 means last node, k=2 means second-to-last).
   * 
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def findKthNode(list: LinkedList, k: Int): Option[Node] = k match {
    case k if (k <= 0 || k > list.length) => None
    case _ => 
      var slow = list.head
      var fast = list.get(k - 1)
      while (fast.isDefined && fast.get.next.isDefined) {
        slow = slow.flatMap(_.next)
        fast = fast.flatMap(_.next)
      }
      slow
    }


    /**
   * Remove Duplicates from Unsorted Linked List
   * Related: LeetCode 1836: Remove Duplicates From an Unsorted Linked List
   * 
   * Given the head of a linked list, remove all duplicate values.
   * Keep only the first occurrence of each value.
   * Example: 1 -> 2 -> 1 -> 3 -> 2 -> null becomes 1 -> 2 -> 3 -> null
   * 
   * Solution 1 (with buffer):
   * Time Complexity: O(n)
   * Space Complexity: O(n)
   * 
   * Solution 2 (without buffer):
   * Time Complexity: O(n²)
   * Space Complexity: O(1)
   */
  def removeDuplicatesWithSet(list: LinkedList): LinkedList = {
    val set = scala.collection.mutable.Set.empty[Int]
    
    var curr = list.head
    var prev: Option[Node] = None

    while(curr.isDefined){
     if (set.contains(curr.get.value))
       prev.foreach(_.next = curr.get.next)
     else
       set.add(curr.get.value)
       prev = curr
      curr = curr.get.next
    }
    list
  }

  def removeDuplicatesInNSq(list: LinkedList) = {
    var temp = list.head

    while(temp.isDefined){
      var preRunner = temp
      var runner = temp.get.next
      
      while(runner.isDefined){
        if (temp.get.value == runner.get.value)
          preRunner.get.next = runner.flatMap(_.next) 
        else
          preRunner = runner  
        runner = runner.flatMap(_.next)
      }
      temp = temp.get.next
    }
    list
  }
}
