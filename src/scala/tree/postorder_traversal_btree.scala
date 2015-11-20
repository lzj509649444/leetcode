package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by luozhenjun on 15/11/19.
 */
object postorder_traversal_btree {
  def postorder[Int](tree: Tree[Int]): List[Int] = tree match {
    case NullNode => Nil
    case TreeNode(v,left,right) => postorder(left) ::: postorder(right) ::: List(v)
  }

  def run(): Unit ={
    println(postorder(TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(4,
          TreeNode(6,NullNode,NullNode),
          TreeNode(7,NullNode,NullNode)
        ),
        TreeNode(5,NullNode,NullNode)
      ),
      TreeNode(
        3,
        TreeNode(8,NullNode,NullNode),
        TreeNode(9,NullNode,NullNode)
      )
    )))
  }
}
