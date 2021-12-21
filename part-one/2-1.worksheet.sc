import scala.annotation.tailrec

object main {
    def fib(n: Int): Int = {
        @tailrec
        def go(prev: Int, cur: Int, n: Int): Int = 
            if (n <= 0) prev
            else go(cur, prev + cur, n - 1)
        
        go(0, 1, n)
    }
}

import main._

fib(2)
fib(3)
fib(0)
fib(5)
fib(6)