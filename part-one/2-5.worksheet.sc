object main {
    def compose[A,B,C](f: B => C, g: A => B): A => C =
        (a: A) => f(g(a))
}

import main._

val double = (a: Int) => a * 2 
val sayIt = (a: Int) => "Its %d!".format(a)

val sayItDouble = compose(sayIt, double)

sayItDouble(2)