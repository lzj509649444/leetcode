package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-21.
 */
object zigzag_level_traversal_btree {
  def zigzag[Int](tree: Tree[Int]): List[List[Int]] = {
    def run[Int](xs: List[List[Tree[Int]]],reverse: Boolean): List[List[Int]] = xs match {
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
        val cc = if(reverse) current.reverse else current

        if(arr.isEmpty) run(tail, !reverse) ::: List(cc)
        else run(tail :+ arr, !reverse) ::: List(cc)


    }
    run(List(List(tree)),false)
  }

  def run(): Unit ={
    println(zigzag(NullNode))
    println(zigzag(TreeNode(1,NullNode,NullNode)))
    println(zigzag(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode))))
    println(zigzag(TreeNode(1,TreeNode(2,NullNode,NullNode),NullNode)))


    println(zigzag(TreeNode(
      1,
      NullNode,
      TreeNode(
        2,
        NullNode,
        TreeNode(3,NullNode,NullNode)
      )
    )))

    println(zigzag(TreeNode(
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
