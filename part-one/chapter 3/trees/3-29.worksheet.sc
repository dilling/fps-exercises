import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def fold[A,B](t: Tree[A], z: A => B)(f: (B, B) => B): B =
        t match {
            case Leaf(v) => z(v)
            case Branch(l, r) => f(fold(l, z)(f), fold(r, z)(f))
        }

    def size[A](t: Tree[A]): Int = 
        fold(t, _ => 1)(1 + _ + _)

    def maximum(t: Tree[Int]): Int = 
        fold(t, v => v)((x, y) => x.max(y))
        

    def depth[A](t: Tree[A]): Int = 
        fold(t, v => 1)((x, y) => (1 + x).max(1 + y))
    
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = 
        fold(t, (x) => Leaf(f(x)): Tree[B])(Branch(_, _))
}

val t: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(6), Leaf(8))))
Tree.size(t)
Tree.maximum(t)
Tree.depth(t)
Tree.map(t)(_ * 2)

