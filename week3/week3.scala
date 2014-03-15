package week3

/*
 * 3.1 CLASS HIERARCHIES
 *
 */

// Implementation of integer sets
// Invariant: parent node always greater than left child, less than right child

abstract class IntSet {
	def contains(x: Int): Boolean
	def incl(x: Int): IntSet
	def union(other: IntSet): IntSet
}

class Empty extends IntSet {
	def contains(x: Int): Boolean = false
	def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
	def union(other: IntSet): IntSet = other
	override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

	def contains(x: Int): Boolean = 
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true

	def incl(x: Int): IntSet = 
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this

	def union(other: IntSet): IntSet = 
		((left union right) union other) incl elem

	override def toString = "{" + left + elem + right + "}"
}

/*
 * 3.3 POLYMORPHISM
 *
 */

trait List[T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty = false
}

class Nil[T] extends List[T] {
	def isEmpty = true
	def head: Nothing = throw new NoSuchElementException("Nil.head")
	def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def nth[T](n: Int, xs: List[T]): T = 
	if (xs.isEmpty) throw new IndexOutOfBoundsException
	else if(n == 0) xs.head
	else nth(n - 1, xs.tail)
