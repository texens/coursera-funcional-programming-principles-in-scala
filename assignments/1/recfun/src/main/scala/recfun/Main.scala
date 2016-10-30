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
    def pascal(c: Int, r: Int): Int = if (c == 0 || r == c) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def check(count: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty && count == 0) {
          true
        } else {
          chars.head match {
            case '(' => check(count + 1, chars.tail)
            case ')' => if (count == 0) false else check(count - 1, chars.tail)
            case _ => check(count, chars.tail)
          }
        }
      }

      check(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty) 0
      else if(money < coins.head) countChange(money, coins.tail)
      else if(money==coins.head) 1 + countChange(money, coins.tail)
      else countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }
