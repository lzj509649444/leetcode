package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-25.
 */
/*
Given a binary tree, find the maximum path sum.
The path may start and end at any node in the tree. For example: Given the below binary tree,
*/
//假设知道如何求解任意节点为root的的最大path sum
//数组最大连续子序列和
object maximum_path_sum {

  def maximum_path(tree: Tree[Int]): Int = {
    var max: Int = 0
    def run(tree: Tree[Int]): Int = tree match {
      case NullNode => 0
      case TreeNode(v,NullNode,NullNode) => v
      case TreeNode(v,left,right) =>
        val l: Int = run(left)
        val r: Int = run(right)
        max = List(v+l, r+l,v+r+l,v,max).max
        List(v+l,v+r,v,0).max
    }
    run(tree)
    return max
  }

  def run()={
    println(maximum_path(TreeNode(
      1,
      TreeNode(-1,
        TreeNode(1,NullNode,NullNode),
        TreeNode(1,NullNode,NullNode)),
      TreeNode(1,
        TreeNode(1,NullNode,NullNode),
        TreeNode(1,NullNode,NullNode))
    )))
  }
}
