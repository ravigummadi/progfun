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
    if(r==0) 
      if(c==0) 1 else 0
    else
      if(c < 0 || r < 0) 0 
      else
        pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
        def balanceCount(chars: List[Char], openBraceCount: Int, closeBraceCount: Int) : Boolean = {
          if(chars.isEmpty){
            if(openBraceCount == closeBraceCount) true
            else false
          }else{
            if(closeBraceCount > openBraceCount) false
            else{
               if(chars.head == '(')
                 balanceCount(chars.tail, openBraceCount+1, closeBraceCount)
               else if(chars.head == ')')
                 balanceCount(chars.tail, openBraceCount, closeBraceCount+1)
               else
                 balanceCount(chars.tail, openBraceCount, closeBraceCount)                   
            }
          }                          
        }          
        balanceCount(chars,0,0)
      }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
	  def count1(money: Int, coins: List[Int]) : Int = {
		  if(money == 0){
			  1
		  }else{
		    if(money < 0){
			    0
		    }else if(coins.isEmpty && money >=1)
		      0
		      else
		        count1(money, coins.tail) + count1(money - coins.head, coins)
		  }
	  }	  
	  count1(money, coins.sortWith(_.compareTo(_) < 0))
  }
}
