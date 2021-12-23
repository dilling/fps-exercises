import scala.annotation.tailrec
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    // Case 1 - We are at the end of the super string and there are more members in the substring
    // Case 2 - Both strings have members. If the current members match, check if the remainder 
    //       of both strings match . Otherwise, compare the substring with the remainder of the super string
    // Case 3 - We have reached the end of the substring. Return true
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
        (sup, sub) match {
            case (Nil, Cons(_, _)) => false
            case (Cons(x1, xs1), Cons(x2, xs2)) => if (x1 == x2 && hasSubsequence(xs1, xs2)) true else hasSubsequence(xs1, sub)
            case (_, Nil) => true
        }
    
    def apply[A](as :A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

val x = List(1, 2, 3, 4, 5, 6)
List.hasSubsequence(x, List(2, 3))
List.hasSubsequence(x, List(3, 2))
List.hasSubsequence(x, List(3, 2, 4))
List.hasSubsequence(x, Nil)
List.hasSubsequence(x, List(1, 2, 3, 4, 5, 6))
List.hasSubsequence(x, List(4, 5, 6))
List.hasSubsequence(x, List(2, 2))
List.hasSubsequence(Nil, Nil)

