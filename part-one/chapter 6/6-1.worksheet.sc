trait RNG {
    def nextInt: (Int, RNG)
    def nonNegativeInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

    def nonNegativeInt: (Int, RNG) =
        nextInt match {
            case (Int.MinValue, next) => nonNegativeInt
            case (x, next) if x < 0 => (-x, next)
            case (x, next) => (x, next)
        }
}

val rng = SimpleRNG(42)
val (x, rng2) = rng.nonNegativeInt
rng.nonNegativeInt
rng2.nonNegativeInt
