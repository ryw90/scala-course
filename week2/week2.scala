/* 
 * 2.1 HIGHER-ORDER FUNCTIONS
 *
 */

// Sum of f() applied to integers between a and b
def sum(f: Int => Int, a: Int, b: Int): Int = 
	if(a > b) 0
	else f(a) + sum(f, a+1, b)

// Use of anonymous functions
def sumInts(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)
def sumFactorials(a: Int, b:Int) = sum(x => if(x==0) 1 else x * fact(x-1), a, b)

// Tail-recursive version
def sum2(f: Int => Int)(a: Int, b: Int): Int = {
	def loop(a: Int, acc: Int): Int = {
		if(a > b) acc
		else loop(a+1, f(a) + acc)
	}
	loop(a, 0)
}

/*
 * 2.2 CURRYING
 *
 */

// Rewrite sum to return a functions
def sum(f: Int => Int): (Int, Int) => Int = {
	def sumF(a: Int, b:Int): Int = 
		if (a > b) 0
		else f(a) + sumF(a + 1, b)
	sumF
}

// Rewrite to take advantage of currying
def sum(f: Int => Int)(a: Int, b: Int): Int = 
	if (a > b) 0 else f(a) + sum(f)(a+1, b)

// Exercise: Write a product function (analagous to sum)
object exercise {
	def product(f: Int => Int)(a: Int, b: Int): Int = 
		if (a > b) 1 else f(a) * product(f)(a+1, b)
	def fact(n: Int): Int = product(x => x)(1, n)

	def reduce(g: Int => Int, base: Int)(f: Int => Int)(a: Int, b:Int): Int = {
		if (a > b) base else g(f(a), reduce(g)(f)(a+1, b))
	}

	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int) {
		if(a > b) zero
		else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
	}

	def product2(f: Int => Int)(a: Int, b: Int): Int = 
		mapReduce(f, (x,y) => x * y, 1)
}

/*
 * 2.3 FINDING FIXED POINTS
 *
 */

import math.abs

object exercise2 {
	val tolerance = 1e-4

	def isCloseEnough(x: Double, y: Double) = 
		abs( (x - y) / x ) / x < tolerance

	def fixedPoint(f: Double => Double)(initGuess: Double) = {
		def iterate(guess: Double): Double = {
			val next = f(guess)
			if(isCloseEnough(guess, next)) next
			else iterate(next)
		}
		iterate(initGuess)
		}
	}

	def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

	def sqrt(x: Double) = fizedPoint(averageDamp(y => x / y))(1)
}

/*
 * 2.5 FUNCTIONS AND DATA / 2.6 MORE FUN WITH RATIONALS
 *
 */

class Rational(x: Int, y: Int) {
	require( y > 0, "denominator must be positive")

	// Separate constructor (only one argument)
	def this(x: Int) = this(x, 1)

	// Store in simplified form
	private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
	private val = gcd(x, y)
	def numer = x / g
	def denom = y / g

	def add(that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)

	def neg = new Rational(-numer, denom)

	def sub(that: Rational) = add(that.neg)

	def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

	def max(that: Rational) = if(this.less(that)) that else this

	override def toString = numer + " / " + denom

}
