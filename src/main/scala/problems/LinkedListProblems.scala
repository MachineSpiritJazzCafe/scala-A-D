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


  /**
 * Binary to Decimal Conversion
 * Related: LeetCode 1017 (Convert to Base -2), General bit manipulation problems
 * 
 * Given a binary string (containing only '0' and '1'), convert it to decimal.
 * Example: "1010" becomes 10, "1111" becomes 15, "10000000" becomes 128
 * 
 * Solution (fold left with accumulator):
 * Time Complexity: O(n) where n is the length of the binary string
 * Space Complexity: O(1)
 */
  def binaryToDecimal(ll: LinkedList): Option[Int] = {
    var result = 0

    var temp = ll.head
    if (temp.isEmpty) return None
    while(temp.isDefined)
      result *= 2
      if (temp.get.value == 1)
        result += 1
      temp = temp.get.next
    Some(result)  
  }
 
  /**
 * Partition List
 * Related: LeetCode 86: Partition List
 * 
 * Partition linked list so all nodes < x come before nodes >= x.
 * Preserve relative order of nodes in each partition.
 * Example: 3->8->5->10->2->1, x=5 becomes 3->2->1->8->5->10
 * 
 * Solution (Two dummy nodes):
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 */
  def partitionList(ll: LinkedList, value: Int): LinkedList = {
    var d1 = Node(0, None)
    var d2 = Node(0, None)
    var p1 = d1 
    var p2 = d2

    var curr: Option[Node] = ll.head
  
    while(curr.isDefined)
      val node = curr.get
      if (node.value < value)
        p1.next = Some(node)
        p1 = node
      else
        p2.next = Some(node)
        p2 = node
      curr = node.next  
    
     p2.next = None
     p1.next = d2.next
     
     ll.head = d1.next
     ll.tail = if (d2.next.isDefined) Some(p2) else Some(p1) 
     d1.next = None
     d2.next = None
     ll
  }

  /**
   * Reverse Between
   * Related: LeetCode 92: Reverse Linked List II
   * 
   * Reverse nodes from start_index to end_index (inclusive, 0-indexed).
   * Example: 1->2->3->4->5, start=2, end=4 becomes 1->2->5->4->3
   * 
   * Solution (One pass with dummy node):
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def reverseBetween(ll: LinkedList, sIdx: Int, eIdx: Int): LinkedList = {
    var dummy: Node = Node(0, ll.head)
    var pre: Option[Node] = Some(dummy)
    
    (0 until sIdx).foreach { _ =>
      pre = pre.flatMap(_.next)
    }

    var curr = pre.flatMap(_.next)
    
    (1 to (eIdx - sIdx)).foreach{ _ =>
      val toMove = curr.flatMap(_.next)
      curr.foreach(_.next = toMove.flatMap(_.next))
      toMove.foreach(_.next = pre.flatMap(_.next))
      pre.foreach(_.next = toMove)
    }
    ll.head = dummy.next
    dummy.next = None
    ll
  }

  /**
   * Swap Nodes in Pairs
   * Related: LeetCode 24: Swap Nodes in Pairs
   * 
   * Swap every two adjacent nodes and return the head.
   * Example: 1->2->3->4->5->6->7 becomes 2->1->4->3->6->5->7
   * 
   * Solution (Iterative with dummy node):
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def swapPairs(ll: LinkedList): LinkedList = {
    if (ll.head.isEmpty || ll.head.flatMap(_.next).isEmpty) return ll
    
    val dummy = Node(0, ll.head)
    var prev: Option[Node] = Some(dummy)

    while(prev.flatMap(_.next).isDefined &&
          prev.flatMap(_.next).flatMap(_.next).isDefined)
      
      val first = prev.flatMap(_.next)
      val second = first.flatMap(_.next)
      
      first.foreach(_.next = second.flatMap(_.next))
      second.foreach(_.next = first)
      prev.foreach(_.next = second)
      prev = first
    
    ll.head = dummy.next
    ll  
  }
}
