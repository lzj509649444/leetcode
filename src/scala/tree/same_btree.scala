package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-27.
 */

/*
Same Tree
描述
Given two binary trees, write a function to check if they are equal or not.
Two binary trees are considered equal if they are structurally identical and the nodes have the same value.
*/

object same_btree {
  def is_same(tree: Tree[Int], other: Tree[Int]): Boolean = (tree,other) match {
    case (NullNode,NullNode) => true
    case (TreeNode(_,_,_),NullNode) => false
    case (NullNode,TreeNode(_,_,_)) => false
    case (TreeNode(v,l,r), TreeNode(o,ol,or)) =>
      v == o && is_same(l,ol) && is_same(r,or)
  }

  def run(): Unit ={
    println(is_same(NullNode,NullNode))
    println(is_same(TreeNode(1,NullNode,NullNode),NullNode))
    println(is_same(NullNode,TreeNode(1,NullNode,NullNode)))
    println(is_same(TreeNode(1,NullNode,NullNode),TreeNode(1,NullNode,NullNode)))
    println(is_same(TreeNode(1,NullNode,NullNode),TreeNode(2,NullNode,NullNode)))
    println(is_same(
      TreeNode(1,TreeNode(1,NullNode,TreeNode(1,NullNode,NullNode)),NullNode),
      TreeNode(1,TreeNode(1,NullNode,NullNode),NullNode)))
  }
}
