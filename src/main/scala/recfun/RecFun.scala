package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c <= r) && (r > 0)) {
      c match {
        case x if x == r || x == 0 => 1
        case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }else 0
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def listBalance(charsList: List[Char],counter : Int): Boolean = {

     charsList match {
       case Nil => counter == 0
       case head :: tail =>
         if (counter >= 0) {
           head match {
             case '(' => listBalance(tail, counter + 1)
             case ')' => listBalance(tail, counter - 1)
             case _ => listBalance(tail, counter)
           }
         } else false
      }
    }
    listBalance(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val maxIndex = coins.length-1

    def amount(difference: Int, index: Int): Int = {
      if (index > maxIndex) {
        0
      } else {
        difference match {
          case 0 => 1
          case x if x < 0 => 0
          case _ => amount(difference - coins(index), index) + amount(difference, index + 1)
        }
      }
    }

    def iteractions(index : Int, sumResult : Int) : Int ={
      if (index <= maxIndex) {
        iteractions(index+1,sumResult+amount(money - coins(index), index))
      } else sumResult
    }
    iteractions(0,0)
  }
}

// этот алгоритм считает количество способов получить результат заданными слогаемыми. результат представляет собой
// сумму слогаемых F(Money - coins[i],i), чтобы осуществить такой перебор реализована функция iteractions,
// благодаря ей значение суммы накапливается т.к. передается в качестве параметра при рекурсивном вызове.
// сам подсчет результата рекурсивного вызова в функции amount
