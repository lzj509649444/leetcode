package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by luozhenjun on 15/11/20.
 */
//从上到下,从左到右,打印每一层的节点值

object level_traversal_btree {

  def level_order[Int](tree: Tree[Int]): List[Int] = {
    def run[Int](xs: List[Tree[Int]]): List[Int] = xs match {
      case Nil => Nil
      case NullNode :: tail => run(tail)
      case TreeNode(v,left,right) :: tail =>
        v :: run(tail :+ left :+ right)
    }
    run(List(tree))
  }

  def run(): Unit ={
    println(level_order(NullNode))
    println(level_order(TreeNode(1,NullNode,NullNode)))
    println(level_order(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode))))
    println(level_order(TreeNode(1,TreeNode(2,NullNode,NullNode),NullNode)))

    println(level_order(TreeNode(
      1,
      NullNode,
      TreeNode(
        2,
        NullNode,
        TreeNode(3,NullNode,NullNode)
      )
    )))

    println(level_order(TreeNode(
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
