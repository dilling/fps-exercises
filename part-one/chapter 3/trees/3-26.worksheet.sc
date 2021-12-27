import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def maximum(t: Tree[Int]): Int = 
        t match {
            case Leaf(v) => v
            case Branch(l, r) => maximum(l).max(maximum(r))
        }
}

val t: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

Tree.maximum(t)