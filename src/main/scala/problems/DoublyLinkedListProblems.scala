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
}

