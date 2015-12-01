package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-12-1.
 */

// root left right
// left root right

object construct_btree_from_preorder_and_inorder {

  def construct(pres: List[Int], ins: List[Int]): Tree[Int] = ins match {
    case Nil => NullNode
    case List(v) => TreeNode(v,NullNode,NullNode)
    case _ =>
      val (v::pre_tail) = pres
      val pivot = ins.indexOf(v)
      val (left,right) = ins.splitAt(pivot)
      TreeNode(
        v,
        construct(pre_tail.take(left.length),left),
        construct(pre_tail.drop(left.length),right.tail))
  }

  def run(): Unit ={
    println(construct(List(1,2,3),List(2,1,3)))
    println(construct(List(1,2),List(1,2)))
    println(construct(List(1,2),List(2,1)))
    println(construct(List(1,2,4,5,3,6,7),List(4,2,5,1,6,3,7)))
    TreeNode(1,
      TreeNode(
        2,
        TreeNode(4,NullNode,NullNode),
        TreeNode(5,NullNode,NullNode)),
      TreeNode(
        3,
        TreeNode(6,NullNode,NullNode),
        TreeNode(7,NullNode,NullNode)))

  }
}
