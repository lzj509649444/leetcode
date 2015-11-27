package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-27.
 */

/*
Balanced Binary Tree
描述
Given a binary tree, determine if it is height-balanced.
For this problem, a height-balanced binary tree is defined as a binary tree in which the depth of the two
subtrees of every node never differ by more than 1.
*/

object balanced_btree {

  def is_balanced(tree: Tree[Int]): Boolean = {

    def run(t: Tree[Int]): Int = t match {
      case NullNode => 0
      case TreeNode(_,l,r) =>
        val bl = run(l)
        if(bl < 0) return -1
        val br = run(r)
        if(br < 0) return -1
        if(Math.abs(bl-br) > 1) return -1
        return Math.max(bl,br) + 1
    }

    val res = run(tree)
    res >= 0
  }

  def run(): Unit ={
    println(is_balanced(NullNode))
    println(is_balanced(TreeNode(1,NullNode,NullNode)))
    println(is_balanced(TreeNode(1,NullNode,TreeNode(1,NullNode,NullNode))))
    println(is_balanced(TreeNode(1,NullNode,TreeNode(1,NullNode,TreeNode(1,NullNode,NullNode)))))
  }
}
