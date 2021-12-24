object main {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = 
        (A, B) => f(A)(B)
}

import main._

val addCurry = (a: Int) => (b: Int) => a + b

val add = uncurry(addCurry)

add(1, 2)