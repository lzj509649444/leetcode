package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-25.
 */

/*Path Sum II
描述
Given a binary tree and a sum, find all root-to-leaf paths where each path’s sum equals the given sum.
For example: Given the below binary tree and sum = 22,
return
[
[5,4,11,2],
[5,8,4,5]
]*/

object path_sum_ii {

  def path(tree: Tree[Int],sum: Int): List[List[Int]] = {

    def run(tree: Tree[Int],xs: List[Int],s: Int): List[List[Int]] = (tree,s) match {
      case (NullNode,0) => List(xs)
      case (NullNode,a) if a != 0 => Nil
      case (TreeNode(v,NullNode,NullNode),ss) =>
        if(ss-v==0)List(xs:+v) else Nil
      case (TreeNode(v,left,right),ss) =>
        val lxs = run(left,xs :+ v, ss-v)
        val rxs = run(right, xs :+ v, ss-v)
        lxs ::: rxs
    }

    run(tree,Nil,sum)
  }

  def run(): Unit ={
    println(path(NullNode,0))

    println(path(TreeNode(1,NullNode,NullNode),1))
    println(path(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode)),3))
    println(path(TreeNode(1,TreeNode(2,NullNode,NullNode),NullNode),3))

    println(path(TreeNode(
      1,
      NullNode,
      TreeNode(
        2,
        NullNode,
        TreeNode(3,NullNode,NullNode)
      )
    ),6))

    println(path(TreeNode(
      1,
      TreeNode(1,
        TreeNode(1,NullNode,NullNode),
        TreeNode(1,NullNode,NullNode)),
      TreeNode(1,
        TreeNode(1,NullNode,NullNode),
        TreeNode(1,NullNode,NullNode))
    ),3))
  }
}
