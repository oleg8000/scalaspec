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
    def pascal(c: Int, r: Int): Int =
      if ((c == 0) || ( r == c))
        1
      else
        pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanceIter(chars: List[Char], pars: List[Char]): Boolean =
        if (chars.isEmpty && pars.isEmpty)
          true
        else if (chars.isEmpty && pars.nonEmpty)
          false
        else if (chars.head == '(')
          balanceIter(chars.tail, '(' +: pars)
        else if ((chars.head == ')') && (pars.isEmpty))
          false
        else if ((chars.head == ')') && (pars.head == '('))
          balanceIter(chars.tail, pars.tail)
        else if ((chars.head == ')') && (pars.head != '('))
          false
        else
          balanceIter(chars.tail, pars)

      balanceIter(chars, List[Char]()) 
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeIter(money: Int, coins: List[Int], acc: Int): Int =
        if (coins.isEmpty || (money == 0))
          0
        else if (money == acc)
          1
        else if (money < acc)
          0
        else
          countChangeIter(money, coins, acc + coins.head) + countChangeIter(money, coins.tail, acc)

      countChangeIter(money, coins, 0)
    }
  }
