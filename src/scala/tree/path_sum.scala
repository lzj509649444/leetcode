package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-25.
 */

/*Given a binary tree and a sum, determine if the tree has a root-to-leaf path such that adding up all the
values along the path equals the given sum.
For example:

return true, as there exist a root-to-leaf path 5->4->11->2 which sum is 22.*/

object path_sum {

  def has_sum(tree: Tree[Int], sum: Int): Boolean = (tree,sum) match {
    case (NullNode,0) =>  true
    case (NullNode,s) if s != 0 => false
    case (TreeNode(v,left,right),s) =>
      has_sum(left,s-v) || has_sum(right,s-v)

  }

  def run(): Unit ={
    println(has_sum(NullNode,0))

    println(has_sum(TreeNode(1,NullNode,NullNode),1))
    println(has_sum(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode)),3))
    println(has_sum(TreeNode(1,TreeNode(2,NullNode,NullNode),NullNode),3))


    println(has_sum(TreeNode(
      1,
      NullNode,
      TreeNode(
        2,
        NullNode,
        TreeNode(3,NullNode,NullNode)
      )
    ),6))

    println(has_sum(TreeNode(
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
    ),14))
  }
}
