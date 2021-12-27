import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = 
        t match {
            case Leaf(v) => Leaf(f(v))
            case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        }
}

val mapTree = Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))

mapTree(_ + 1)
mapTree(_ * 2)