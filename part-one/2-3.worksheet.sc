object main {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        A => B => f(A, B)
}

import main._

val add = (a: Int, b: Int) => a + b

val addCurry = curry(add)

addCurry(1)(2)