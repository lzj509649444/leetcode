package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-28.
 */
object copy_btree {

  def copy(tree: Tree[Int]): Tree[Int] = tree match {
    case NullNode => NullNode
    case TreeNode(v,l,r) =>
      TreeNode(v,copy(l),copy(r))
  }

  def run(): Unit ={
    println(copy(NullNode))
    println(copy(TreeNode(1,NullNode,NullNode)))
    println(copy(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode))))
  }
}
