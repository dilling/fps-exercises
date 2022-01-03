trait RNG {
    def nextInt: (Int, RNG)
}

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG {
    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        rng => {
            val (a, rng2) = ra(rng)
            val (b, rng3) = rb(rng2)
            (f(a, b), rng3)
        }
    
    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
        map2(ra, rb)((_,_))

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
        rng => {
            fs.foldLeft((Nil: List[A], rng))((acc, a) => {
                val (x, rng2) = a(acc._2)
                (x :: acc._1, rng2)
            })
        }

    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        val fx = List.fill(count)((x: RNG) => x.nextInt)
        sequence(fx)(rng)
}

val rng = SimpleRNG(42)
rng.ints(4)(rng)

