sealed trait Option[+A] {
    def flatMap[B](f: A => Option[B]): Option[B] =
        this match {
            case Some(x) => f(x)
            case None => None
        }
}
case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing]

object main {
    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.size != 0) Some(xs.sum / xs.size)
        else None
    
    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs).flatMap(m => Some(xs.map(x => math.pow(x - m, 2)))).flatMap(mean)
        // xs.size match {
        //     case 0 => None
        //     case s => {
        //         val mean = xs.sum / s
        //         Some(xs.foldLeft(0.0)((x,y) => x + math.pow(y - mean, 2)) / s)
        //     }
        // }
    }
}

val x = Seq(1.0, 2.0, 5.0)
main.variance(x)

val y = Seq()
main.variance(y)
