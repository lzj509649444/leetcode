package scala.ADT

/**
 * Created by luozhenjun on 15/11/18.
 */
/*
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class TreeNode[A](left: Tree[A], right: Tree[A]) extends Tree[A]
*/

sealed trait Tree[+A]
case object NullNode extends Tree[Nothing]
case class TreeNode[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
case class TreeNodeWithNext[A](value: A, left: Tree[A],right: Tree[A], next: Tree[A]) extends Tree[A]

/*

trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]


scala> Node("foo", Node("bar", Empty, Empty), Empty)
res4: Node[String] = Node(foo,Node(bar,Empty,Empty),Empty)


scala> :paste
// Entering paste mode (ctrl-D to finish)

import scala.math.max
def depth[A](tree: Tree[A]): Int = tree match {
  case Empty => 0
  case Node(_, left, right) => 1 + max(depth(left), depth(right))
}

// Exiting paste mode, now interpreting.

import scala.math.max
depth: [A](tree: Tree[A])Int

scala> depth(Node("foo", Node("bar", Empty, Empty), Empty))
res5: Int = 2


1) You can make it a standalone function external to the trait:

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def depth[A](tree: Tree[A]): Int = tree match {
    case Empty => 0
    case Node(_, left, right) => 1 + max(depth(left), depth(right))
  }
}

2) You can make it a concrete method of the trait itself:

sealed trait Tree[+A] {
  def depth: Int = this match {
    case Empty => 0
    case Node(_, left, right) => 1 + max(left.depth, right.depth)
  }
}
case object Empty extends Tree[Nothing]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]


3) You can make it an abstract method of the trait with concrete implementations in the subtypes (obviating the need to use match):

sealed trait Tree[+A] {
  def depth: Int
}
case object Empty extends Tree[Nothing] {
  val depth = 0
}
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
  def depth = 1 + max(left.depth, right.depth)
}

This is most similar to the approach traditional object-oriented polymorphism. It feels natural to me when defining the low-level operations of the trait, with richer functionality defined in terms of these operations in the trait itself. It's also most appropriate when working with traits that aren't sealed.

4) Or, in the case you want to add a method to an ADT whose source is external to your project, you could use an implicit conversion to a new type that has the method:

// assuming Tree defined elsewhere
implicit class TreeWithDepth[A](tree: Tree[A]) {
  def depth: Int = tree match {
    case Empty => 0
    case Node(_, left, right) => 1 + max(left.depth, right.depth)
  }
}


scala> Node("foo", Node("bar", Empty, Empty), Empty).depth
res8: Int = 2


*/