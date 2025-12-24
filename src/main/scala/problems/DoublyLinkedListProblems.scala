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
}

