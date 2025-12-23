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
}

