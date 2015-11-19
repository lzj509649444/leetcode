package scala

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by luozhenjun on 15/11/19.
 */
object minimum_depth_of_btree {

  def minimum_depth[A](tree: Tree[A]): Int = {
    def depth(root: Tree[A], sibling: Tree[A]): Int = root match {
      case NullNode =>
        sibling match {
          case NullNode => 0
          case _ => Int.MaxValue
        }
      case TreeNode(_, left,right) => utils.min(depth(left,right),depth(right,left)) + 1
    }
    depth(tree,NullNode)
  }

  def run(): Unit ={
    println(minimum_depth(NullNode))
    println(minimum_depth(TreeNode(0,NullNode,NullNode)))
    println(minimum_depth(TreeNode(0,NullNode,TreeNode(0,NullNode,NullNode))))
    println(minimum_depth(TreeNode(0,TreeNode(0,NullNode,NullNode),NullNode)))

    println(minimum_depth(TreeNode(
      0,
      NullNode,
      TreeNode(
        0,
        NullNode,
        TreeNode(0,NullNode,NullNode)
      )
    )))

    println(minimum_depth(TreeNode(
      0,
      NullNode,
      TreeNode(
        0,
        NullNode,
        TreeNode(
          0,
          TreeNode(0,NullNode,NullNode),
          NullNode)
      )
    )))


    println(minimum_depth(
      TreeNode(
        0,

        TreeNode(
          0,
          TreeNode(
            0,
            TreeNode(0,NullNode,NullNode),
            NullNode
          ),
          NullNode
        ),

        TreeNode(
          0,
          NullNode,
          TreeNode(
            0,
            NullNode,
            TreeNode(0,
              TreeNode(0,NullNode,NullNode),
              NullNode)
          )
        )
      ))
    )
  }
}
