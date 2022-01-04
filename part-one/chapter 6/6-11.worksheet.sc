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

type Inc = State[Int, Int]
val i: Inc = State(s => (s, s+1))
i.run(0)
i.get.run(0)
i.set(3).run(0)