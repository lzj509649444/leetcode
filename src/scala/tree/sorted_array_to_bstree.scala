package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-12-1.
 */
//Convert Sorted Array to Binary Search Tree

object sorted_array_to_bstree {

  def array_to_bstree(xs: List[Int]): Tree[Int] = xs match {
    case Nil => NullNode
    case List(v) => TreeNode(v,NullNode,NullNode)
    case _ =>
      val len = xs.length
      val (left,right) = xs.splitAt(len/2)
      val tl = left match {
        case Nil => NullNode
        case _ => array_to_bstree(left)
      }
      right match {
        case Nil => NullNode
        case List(v) => TreeNode(v,tl,NullNode)
        case head :: tail => TreeNode(head,tl,array_to_bstree(tail))
      }
  }

  def run(): Unit ={
    println(array_to_bstree(List(1)))
    println(array_to_bstree(List(1,2)))
    println(array_to_bstree(List(1,2,3)))
    println(array_to_bstree(List(1,2,3,4)))
  }
}
