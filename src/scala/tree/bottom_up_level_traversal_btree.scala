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
        val arr = h.flatMap{ e: Tree[Int] =>
          e match {
            case NullNode => Nil
            case TreeNode(v,left,right) => List(left,right)
          }
        }
        val v= arr.map(_.asInstanceOf[TreeNode].value)
        v match {
          case Nil => run(tail)
          case _ => run(tail :+ arr)
        }

    }
  }
}
