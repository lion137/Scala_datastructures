sealed trait Tree[+A]
case object EmptyTree extends Tree[Nothing]
case class Node[A](value: A , left: Tree[A], right: Tree[A]) extends Tree[A]
