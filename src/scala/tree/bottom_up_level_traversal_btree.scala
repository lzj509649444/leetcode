package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by luozhenjun on 15/11/20.
 */

//从下到上,从左到右,打印每一层的节点值

object bottom_up_level_traversal_btree {
  def bottom_up[Int](tree: Tree[Int]): List[List[Int]] = {
    def run[Int](xs: List[List[Tree[Int]]]): List[List[Int]] = xs match {
      case Nil => Nil
      case (h: List[Tree[Int]]) :: tail =>

        //考虑清楚所有的匹配
        val arr = h.flatMap{ e: Tree[Int] =>
          e match {
            case NullNode => Nil
            case TreeNode(v,NullNode,NullNode) => Nil
            case TreeNode(v,l: TreeNode[Int],NullNode) => List(l)
            case TreeNode(v,NullNode,r: TreeNode[Int]) => List(r)
            case TreeNode(v,left: TreeNode[Int],right: TreeNode[Int]) => List(left,right)
          }
        }


        val current = h.flatMap{e: Tree[Int] =>
          e match {
            case NullNode => Nil
            case TreeNode(v,l,r) => List(v)
          }
        }
        if(arr.isEmpty) current  :: run(tail)
        else current :: run(tail :+ arr)


    }
    run(List(List(tree))).reverse
  }


  def run(): Unit ={
    println(bottom_up(NullNode))
    println(bottom_up(TreeNode(1,NullNode,NullNode)))
    println(bottom_up(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode))))
    println(bottom_up(TreeNode(1,TreeNode(2,NullNode,NullNode),NullNode)))


    println(bottom_up(TreeNode(
      1,
      NullNode,
      TreeNode(
        2,
        NullNode,
        TreeNode(3,NullNode,NullNode)
      )
    )))

    println(bottom_up(TreeNode(
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
