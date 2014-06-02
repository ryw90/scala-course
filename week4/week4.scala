package week4

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

def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if(x < y) x :: xs else y :: insert(x, ys)
}
