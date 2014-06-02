import math.Ordering

def msort(xs: List[Int])(implicit ord: Ordering): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
        def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (ord.lt(x, y)) x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
        }
        val (fst, snd) = xs splitAt n
        merge(msort(fst), msort(snd))
    }
}

abstract class List[T] {
    def map[U](f: T => U): List[U] = this match {
        case Nil => this
        case x :: xs => f(x) :: xs.map(f)
    }
}

def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
        val (fst, snd) = xs1 span (y => x == y)
        ( x :: fst) :: pack(snd)
    }
}

def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (x => (x(0), x.size))

def mapFun[T,U](xs: List[T], f: T => U) =
    (xs foldRight List[U]())((x, z) => f(x) :: z)

def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_, z) => z + 1)
