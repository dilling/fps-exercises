case class State[S,+A](run: S => (A,S)) {
    def unit[A](a: A): State[S,A] =
        State(s => (a, s))

    def flatMap[B](f: A => State[S,B]): State[S, B] =
        State(s => {
            val (a, s2) = run(s)
            f(a).run(s2)
        })

    def map[B](f: A => B): State[S, B] =
        flatMap(a => State(s => (f(a), s)))

    def map2[B,C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
        flatMap(a => 
            rb.flatMap(b => State(s => (f(a, b), s)))
        )

    def get: State[S, S] = State(s => (s, s))
    def set(s: S): State[S, Unit] = State(_ => ((), s))
    def modify(f: S => S): State[S, Unit] = for {
        s <- get
        _ <- set(f(s))
    } yield()
}

object State {
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
        State(s => 
            fs.foldLeft((Nil: List[A], s))((acc, curState) => {
                val (a, nextState) = curState.run(acc._2)
                (a :: acc._1, nextState)
            }))
}

sealed trait Input 
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
        State(m => {
            val machine = inputs.foldLeft(m)((acc, input) => 
                input match {
                    case Coin => Machine(false, acc.candies, acc.coins+1)
                    case Turn => Machine(true, (acc.candies - 1).max(0), acc.coins)
                })

            ((machine.coins, machine.candies), machine)
        })
    }
}

val machine = Machine(true, 5, 10)
val (a, b) = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(machine)
a
b
