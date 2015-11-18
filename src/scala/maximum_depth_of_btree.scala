package scala

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by luozhenjun on 15/11/18.
 */
object maximum_depth_of_btree {
  def depth[A](tree: Tree[A]): Int = tree match {
    case NullNode => 0
    case TreeNode(_,left,right)  => 1 + utils.max(depth(left),depth(right))
  }

  def depth[A](tree: Tree[A], dp: Int): Int = tree match {
    case NullNode => dp
    case TreeNode(_,left,right) => utils.max( depth(left,dp+1), depth(right,dp+1) )
  }

  def run(): Unit ={
    println(depth(TreeNode(0,NullNode,NullNode)))
    println(depth(NullNode))
    println(depth(TreeNode(0,NullNode,TreeNode(0,NullNode,NullNode))))
    println(depth(TreeNode(0,NullNode,TreeNode(0,NullNode,NullNode))))
    println(depth(TreeNode(0,
      TreeNode(0,
        TreeNode(0,NullNode,NullNode),
        TreeNode(0,NullNode,NullNode)
      ),
      TreeNode(0,NullNode,NullNode)
    )
    ))

    println()

    println(depth(TreeNode(0,NullNode,NullNode),0))
    println(depth(NullNode))
    println(depth(TreeNode(0,NullNode,TreeNode(0,NullNode,NullNode)),0))
    println(depth(TreeNode(0,NullNode,TreeNode(0,NullNode,NullNode)),0))
    println(depth(TreeNode(0,
      TreeNode(0,
        TreeNode(0,NullNode,NullNode),
        TreeNode(0,NullNode,NullNode)
      ),
      TreeNode(0,NullNode,NullNode)
    ),0
    ))

    val t = TreeNode(0,NullNode,TreeNode(0,NullNode,NullNode))
    println(t.value)
    println(t.left)

  }
}
