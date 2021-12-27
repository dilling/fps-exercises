import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def depth[A](t: Tree[A]): Int = 
        t match {
            case Leaf(v) => 1
            case Branch(l, r) => (1 + depth(l)).max(1 + depth(r))
        }
}

Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
Tree.depth(Branch(Leaf(1), Leaf(1)))
Tree.depth(Leaf(1))