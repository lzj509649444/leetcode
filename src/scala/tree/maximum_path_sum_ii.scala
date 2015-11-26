package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-26.
 */
// any node to any node
object maximum_path_sum_ii {
  def maximum_path_ii(tree: Tree[Int]): Int = {
    var max: Int = 0
    def run(tree: Tree[Int]): Int = tree match {
      case NullNode => 0
      case TreeNode(v,NullNode,NullNode) => v
      case TreeNode(v,left,right) =>
        val l: Int = run(left)
        val r: Int = run(right)
        max = List(v+l, r+l,v+r+l, v,max).max
        List(v+l,v+r,v,v+r+l,0).max
    }
    run(tree)
    return max
  }

  def run()={
    println(maximum_path_ii(TreeNode(
      1,
      TreeNode(-1,
        TreeNode(1,NullNode,NullNode),
        TreeNode(1,NullNode,NullNode)),
      TreeNode(-1,
        TreeNode(1,NullNode,NullNode),
        TreeNode(1,NullNode,NullNode))
    )))
  }
}
