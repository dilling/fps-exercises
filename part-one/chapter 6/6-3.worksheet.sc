trait RNG {
    def nextInt: (Int, RNG)
    def nonNegativeInt(rng: RNG): (Int, RNG)
    def double(rng: RNG): (Double, RNG)
    def intDouble(rng: RNG): ((Int, Double), RNG)
    def doubleInt(rng: RNG): ((Double, Int), RNG)
    def double3(rng: RNG): ((Double, Double, Double), RNG)
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
            case (n, next) => (n.toDouble / Int.MaxValue, next)
        }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, rng2) = rng.nextInt
        val (d, rng3) = rng2.double(rng2)
        ((i, d), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (d, rng2) = rng.double(rng)
        val (i, rng3) = rng2.nextInt
        ((d, i), rng3)
    }
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, rng2) = rng.double(rng)
        val (d2, rng3) = rng2.double(rng2)
        val (d3, rng4) = rng3.double(rng3)
        ((d1, d2, d3), rng4)
    }
}

val rng = SimpleRNG(42)
val ((i, d), rng2) = rng.intDouble(rng)
val ((d2, i2), rng3) = rng2.doubleInt(rng2)
val ((d3, d4, d5), rng4) = rng2.double3(rng3)
