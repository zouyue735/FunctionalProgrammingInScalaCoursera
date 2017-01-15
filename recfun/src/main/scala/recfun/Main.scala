package recfun

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
      if (c == 0) 1 else if (c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def inner(count: Int, chars: List[Char]): Boolean = {
        if (count < 0) false else if (chars.isEmpty) count == 0 else inner(add(count, chars.head), chars.tail)
      }
      def add(count: Int, head: Char): Int = {
        if (head == '(') count + 1 else if (head == ')') count - 1 else count
      }
      inner(0, chars)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countInner(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0) 0
        else if (coins.isEmpty) 0
        else countInner(money - coins.head, coins) + countInner(money, coins.tail)
      }
      countInner(money, coins)
    }
  }
