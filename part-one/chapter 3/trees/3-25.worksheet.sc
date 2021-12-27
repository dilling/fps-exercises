import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](t: Tree[A]): Int = 
        t match {
            case Leaf(_) => 1 
            case Branch(l, r) => 1 + size(l) + size(r)
        }
}

val t: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

Tree.size(t)