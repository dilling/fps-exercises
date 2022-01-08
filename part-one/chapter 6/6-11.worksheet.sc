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
case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
        val machine = inputs.foldLeft(this)((m, input) => 
            input match {
                case Coin =>
                    m match {
                        case Machine(_, ca, co) => Machine(false, ca, co+1) // coin unlocks the machine
                    }
                case Turn => 
                    m match {
                        case Machine(false, ca, co) if ca > 0 => Machine(true, ca-1, co)// turning an unlocked machine with candy returns a candy and locks it
                        case _ => m
                    }
            })
        
        State(_ => ((machine.coins, machine.candies), machine))
    }
}

val machine = Machine(true, 5, 10)
val (a, b) = machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(machine)
a
b