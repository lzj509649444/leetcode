package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-12-1.
 */
object construct_btree_from_inorder_and_postorder {

  def construct(ins: List[Int], poxs: List[Int]): Tree[Int] = ins match {
    case Nil => NullNode
    case List(v) => TreeNode(v,NullNode,NullNode)
    case _ =>
      val (head :+ v) = poxs
      val pivot = ins.indexOf(v)
      val (left,right) = ins.splitAt(pivot)
      TreeNode(
        v,
        construct(left,head.take(left.length)),
        construct(right.tail,head.drop(left.length)))
  }

  def run(): Unit ={
    println(construct(List(2,1,3),List(2,3,1)))
    println(construct(List(1,2),List(2,1)))
    println(construct(List(2,1),List(2,1)))
    println(construct(List(4,2,5,1,6,3,7),List(4,5,2,6,7,3,1)))
    TreeNode(
      1,
      TreeNode(
        5,
        TreeNode(4,NullNode,NullNode),
        TreeNode(2,NullNode,NullNode)),
      TreeNode(7,TreeNode(6,NullNode,NullNode),TreeNode(3,NullNode,NullNode)))

  }

}
