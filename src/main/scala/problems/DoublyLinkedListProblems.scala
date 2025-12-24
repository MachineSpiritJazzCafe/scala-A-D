package problems

import datastructures.{DoublyLinkedList, BiNode}

object DoublyLinkedListProblems {

  /**
   * Palindrome Checker (Doubly Linked List)
   * Related: LeetCode 234: Palindrome Linked List
   * 
   * Determine if a doubly linked list is a palindrome.
   * A palindrome reads the same forward and backward.
   * Example: 1->2->3->2->1 is a palindrome, 1->2->3 is not
   * 
   * Solution (Two pointers from both ends):
   * Time Complexity: O(n/2) = O(n)
   * Space Complexity: O(1)
   */
  def palindromeChecker(dll: DoublyLinkedList): Boolean = dll.length match {
    case l if (l <= 0) => false
    case len =>
      var p1 = dll.head
      var p2 = dll.tail
      (0 until len / 2).forall { _ =>
        val isPalindrome = p1.get.value == p2.get.value
        p1 = p1.flatMap(_.next)
        p2 = p2.flatMap(_.prev) 
        isPalindrome
      }
    }

 /**
 * Reverse Doubly Linked List
 * Related: LeetCode 206: Reverse Linked List (adapted for DLL)
 * 
 * Reverse a doubly linked list in place by swapping next and prev pointers.
 * Example: 1<->2<->3<->4<->5 becomes 5<->4<->3<->2<->1
 * 
 * Solution (Swap next and prev pointers):
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 */
  def reverse(dll: DoublyLinkedList): DoublyLinkedList = {
    var temp = dll.head
    while(temp.isDefined)
      val pre = temp.get.prev
      val next = temp.get.next
      temp.foreach(_.prev = next)
      temp.foreach(_.next = pre)
      temp = next
    val oldHead = dll.head
    dll.head = dll.tail
    dll.tail = oldHead
    dll
  }

  
  /**
   * Partition Doubly Linked List
   * Related: LeetCode 86: Partition List (adapted for DLL)
   * 
   * Partition list so all nodes < x come before nodes >= x.
   * Preserve relative order of nodes in each partition.
   * Example: 3<->8<->5<->10<->2<->1, x=5 becomes 3<->2<->1<->8<->5<->10
   * 
   * Solution (Two dummy nodes):
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def partition(dll: DoublyLinkedList, x: Int): DoublyLinkedList = {
    val d1 = BiNode(0)
    val d2 = BiNode(0)
    var d1p: Option[BiNode] = Some(d1) 
    var d2p: Option[BiNode] = Some(d2)
    var temp = dll.head

    while (temp.isDefined) {
      if (temp.get.value < x) {
        d1p.foreach(_.next = temp)
        temp.foreach(_.prev = d1p)
        d1p = d1p.flatMap(_.next)
      } else {
        d2p.foreach(_.next = temp)
        temp.foreach(_.prev = d2p)
        d2p = d2p.flatMap(_.next)
      }
      temp = temp.flatMap(_.next)
    }
    
    d2p.foreach(_.next = None)
    d1p.foreach(_.next = d2.next)
    d2.next.foreach(_.prev = d1p)
    
    dll.head = d1.next
    dll.head.foreach(_.prev = None)
    dll.tail = if (d2.next.isDefined) d2p else d1p
    dll
  }
  
  /**
   * Reverse Between (Doubly Linked List)
   * Related: LeetCode 92: Reverse Linked List II (adapted for DLL)
   * 
   * Reverse nodes from startIdx to endIdx (inclusive, 0-indexed).
   * Example: 1<->2<->3<->4<->5, start=1, end=3 becomes 1<->4<->3<->2<->5
   * 
   * Solution (One pass with dummy node):
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def reverseBetween(dll: DoublyLinkedList, sIdx: Int, eIdx: Int): DoublyLinkedList = {
    if (dll.head.isEmpty) return dll
    
    val dummy = BiNode(0, prev = None, next = dll.head)
    dll.head.foreach(_.prev = Some(dummy))
    
    var pre: Option[BiNode] = Some(dummy)

    // Move pre to node before startIdx
    (0 until sIdx).foreach { _ =>
      pre = pre.flatMap(_.next)
    }
    
    var curr = pre.flatMap(_.next)

    // Reverse from sIdx to eIdx
    (0 until (eIdx - sIdx)).foreach { _ =>
      val toMove = curr.flatMap(_.next)
      val afterToMove = toMove.flatMap(_.next)
      val preNext = pre.flatMap(_.next)
      
      // Remove toMove from its position
      curr.foreach(_.next = afterToMove)
      afterToMove.foreach(_.prev = curr)
      
      // Insert toMove between pre and preNext
      toMove.foreach(_.next = preNext)
      toMove.foreach(_.prev = pre)
      preNext.foreach(_.prev = toMove)
      pre.foreach(_.next = toMove)
    }
    
    dll.head = dummy.next
    dll.head.foreach(_.prev = None)
    dll
  }

  /**
   * Swap Pairs (Doubly Linked List)
   * Related: LeetCode 24: Swap Nodes in Pairs (adapted for DLL)
   * 
   * Swap every two adjacent nodes and return the head.
   * Example: 1<->2<->3<->4<->5 becomes 2<->1<->4<->3<->5
   * 
   * Solution (Iterative with dummy node):
   * Time Complexity: O(n)
   * Space Complexity: O(1)
   */
  def swapPairs(dll: DoublyLinkedList) = {
    if (dll.length <= 1) dll
    else 
      val dummy = BiNode(0, next = dll.head, prev = None)
      var pre: Option[BiNode] = Some(dummy) 
      var first = pre.flatMap(_.next)
      while(first.isDefined && first.flatMap(_.next).isDefined)
        val second = first.flatMap(_.next)

        first.foreach(_.next = second.flatMap(_.next))
        second.foreach(_.next = first)
        
        first.foreach(_.prev = second)
        second.foreach(_.prev = pre)

        pre.foreach(_.next = second)

        pre = first
        first = first.flatMap(_.next)

      dll.head = dummy.next
      dummy.next = None
      dll
  }
}

