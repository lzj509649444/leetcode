package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-27.
 */

/*
给定一颗二叉树，判断该二叉树是否是对称的。
Given a binary tree, check whether it is a mirror of itself (ie, symmetric around its center).
*/


object symmetric_btree {
  def is_symmetric(tree: Tree[Int]): Boolean = {

    def run(left: Tree[Int], right: Tree[Int]): Boolean = (left,right) match {
      case (NullNode,NullNode) => true
      case (TreeNode(_,_,_),NullNode) => false
      case (NullNode,TreeNode(_,_,_)) => false
      case (TreeNode(lv,ll,lr),TreeNode(rv,rl,rr)) =>
        lv == rv && run(ll,rl) && run(lr,rr)
    }

    tree match {
      case NullNode => true
      case TreeNode(_,NullNode,NullNode) => true
      case TreeNode(_,l,r) => run(l,r)
    }

  }

  def run(): Unit ={
    println(is_symmetric(NullNode))
    println(is_symmetric(TreeNode(1,NullNode,NullNode)))
    println(is_symmetric(TreeNode(1,TreeNode(1,NullNode,NullNode),NullNode)))
    println(is_symmetric(TreeNode(1,NullNode,TreeNode(1,NullNode,NullNode))))
  }
}
