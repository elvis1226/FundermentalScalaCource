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
        if (c == 0 || c == r) 1
        else pascal(c-1,r-1) + pascal(c,r-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def check (l: Int, r: Int, cs : List[Char]) : Boolean = {
            if (cs.isEmpty && l == r) true
            else if (cs.isEmpty && l != r) false
            else if (cs.head ==')' && l == 0) false
            else if (cs.head ==')' && l > 0) check(l-1, r, cs.tail)
            else if (cs.head =='(') check(l+1, r, cs.tail)
            else check(l, r, cs.tail)
        }
        check(0, 0, chars)

    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

        def count(start: Int, change: Int, cns: List[Int]) :Int = {
            if (change == 0) 1
            else if (change < 0) 0
            else if (cns.isEmpty) 0
            else if (start > money) 0
            else if (cns.tail.isEmpty && start*cns.head == change )  1
            else if (cns.tail.isEmpty && start * cns.head > change) 0
            else count(0, change - start* cns.head, cns.tail) + count(start+1, change, cns)

        }
        count(0, money, coins)
    }
  }
