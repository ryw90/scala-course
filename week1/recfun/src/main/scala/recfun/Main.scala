package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(r == 0 & c==0) 1
    else if(c > r || c < 0 || r < 0) 0
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    // A string with no parentheses is balanced
    // Any opening parenthesis is balanced IFF there is a closing parenthesis
    // Any closing parenthesis is balanced IFF there is an opening parenthesis
    // An expression is balanced IFF all of the parentheses are balanced

    // Use an accumulator that can't go negative and must end at zero
    def inner(acc: Int, rest: List[Char]): Boolean = {
      if(rest.isEmpty) acc==0
      else if(acc < 0) false
      else if(rest.head=='(') inner(acc+1, rest.tail)
      else if(rest.head==')') inner(acc-1, rest.tail)
      else inner(acc, rest.tail)
    }  

    inner(0, chars)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // Base cases: 0 ways if amt reaches 0, 0 ways if no coins left

    if(money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)

  }
}
