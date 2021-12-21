object main {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        def loop(n: Int): Boolean =
            if (n + 1 >= as.length) true
            else if (ordered(as(n), as(n + 1))) loop(n + 1)
            else false

        loop(0)
    }
}

import main._

isSorted(Array(1, 2, 3), (i, j) => i <= j)
isSorted(Array(1, 3, 2), (i, j) => i <= j)

isSorted(Array("a", "b", "c"), (i, j) => i <= j)
isSorted(Array("b", "a", "c"), (i, j) => i <= j)