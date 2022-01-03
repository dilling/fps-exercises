trait RNG {
    def nextInt: (Int, RNG)
    def nonNegativeInt(rng: RNG): (Int, RNG)
    def double(rng: RNG): (Double, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) =
        rng.nextInt match {
            case (Int.MinValue, next) => nonNegativeInt(next)
            case (x, next) if x < 0 => (-x, next)
            case (x, next) => (x, next)
        }

    def double(rng: RNG): (Double, RNG) =
        rng.nonNegativeInt(rng) match {
            case (Int.MaxValue, next) => double(next)
            case (n, next) => (n.toDouble / Int.MaxValue, next)
        }
}

val rng = SimpleRNG(42)
val (x, rng2) = rng.double(rng)
rng2.double(rng2)