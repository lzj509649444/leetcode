package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by luozhenjun on 15/11/19.
 */
object preorder_traversal_btree {
  def preorder[Int](tree: Tree[Int]): List[Int] = tree match {
    case NullNode => Nil
    case TreeNode(v,left,right) => (v :: preorder(left)) ::: preorder(right)
  }

  def run(): Unit ={
    println(preorder(NullNode))
    println(preorder(TreeNode(1,NullNode,NullNode)))
    println(preorder(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode))))
    println(preorder(TreeNode(1,TreeNode(2,NullNode,NullNode),NullNode)))

    println(preorder(TreeNode(
      1,
      NullNode,
      TreeNode(
        2,
        NullNode,
        TreeNode(3,NullNode,NullNode)
      )
    )))

    println(preorder(TreeNode(
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
