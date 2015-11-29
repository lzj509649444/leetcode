package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-27.
 */

/*
Given a binary tree, flatten it to a linked list in-place.
For example, Given
     1
    / \
   2   5
  / \   \
 3   4   6
The flattened tree should look like:
1
 \
  2
   \
    3
     \
      4
       \
        5
         \
          6
*/

object flatten_btree_to_linkedlist {

  def flatten(tree: Tree[Int]): List[Int] = tree match {
    case NullNode => Nil
    case TreeNode(v,l,r) =>
      (v :: flatten(l)) ::: flatten(r)
  }

  def run(): Unit ={
    println(flatten(NullNode))
    println(flatten(TreeNode(1,NullNode,NullNode)))
    println(flatten(TreeNode(1,TreeNode(2,NullNode,NullNode),NullNode)))
    println(flatten(TreeNode(1,TreeNode(2,NullNode,NullNode),TreeNode(3,NullNode,NullNode))))
    println(flatten(TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(3,NullNode,NullNode),
        TreeNode(4,NullNode,NullNode)),
      TreeNode(
        5,
        NullNode,
        TreeNode(
          6,
          NullNode,
          NullNode
        )))))
    println(flatten(TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(3,NullNode,NullNode),
        TreeNode(4,NullNode,NullNode)),
      TreeNode(
        5,
        NullNode,
        TreeNode(
          6,
          TreeNode(7,NullNode,NullNode),
          TreeNode(8,NullNode,NullNode)
        )))))
  }
}
