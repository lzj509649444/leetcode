package scala.tree

import scala.ADT.{TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-30.
 */

//Validate Binary Search Tree

//WARING

//bottom_up写法会有陷阱 TreeNode(1,NullNode,TreeNode(2,TreeNode(1,NullNode,NullNode),NullNode))) -> true
//容易忽略所有左子树的值要小于右子树的值，反之，也是一样

//左子树是不是bstree && 右子树是不是bstree && 根节点大于左子树最大值 && 根节点小于右子树最小值
//or
//根节点大于左子树最大值 && 根节点小于右子树最小值

object validate_bstree {

  def validate_bottom_up_has_error(tree: Tree[Int]): Boolean = {

    def run(t: Tree[Int]): (Int, Boolean) = t match {
      case NullNode => (0,true)
      case TreeNode(v,NullNode,NullNode) => (v,true)
      case TreeNode(v,NullNode,r: TreeNode[Int]) =>
        val rr = run(r)
        (v,rr._1 > v)
      case TreeNode(v,l: TreeNode[Int],NullNode) => (v,run(l)._1 < v)
      case TreeNode(v,l,r) =>
        val a = run(l)
        val b = run(r)
        if(!a._2 || !b._2) return (v,false)
        (v, a._1 < v && v < b._1)
    }

    run(tree)._2
  }

  def validate_bottom_up(tree: Tree[Int]): Boolean = {

    def run(t: Tree[Int]): (Int, Int,Boolean) = t match {
      case NullNode => (0,0,true)
      case TreeNode(v,NullNode,NullNode) => (v,v,true)
      case TreeNode(v,NullNode,r: TreeNode[Int]) =>
        val rr = run(r)
        if(!rr._3) return (v,v,false)
        (Math.min(v,rr._1),Math.max(v,rr._2),rr._1 > v)
      case TreeNode(v,l: TreeNode[Int],NullNode) =>
        val rr = run(l)
        if(!rr._3) return (v,v,false)
        (Math.min(v,rr._1),Math.max(v,rr._2),rr._2 < v)
      case TreeNode(v,l,r) =>
        val a = run(l)
        val b = run(r)
        if(!a._3 || !b._3) return (v,v,false)
        (List(v,a._1,b._1).min, List(v,a._2,b._2).max, a._2 < v && v < b._1)
    }

    run(tree)._3
  }

  def validate_top_down(tree: Tree[Int]): Boolean = {

    def run(t: Tree[Int],min: Int,max: Int): Boolean = t match {
      case NullNode => true
      case TreeNode(v,l,r) => v > min && v < max && run(l,min,v) && run(r,v,max)
    }

    run(tree,Int.MinValue,Int.MaxValue)
  }

  def run(): Unit ={
    println(validate_bottom_up(NullNode))
    println(validate_top_down(NullNode))
    println(validate_bottom_up(TreeNode(1,NullNode,NullNode)))
    println(validate_top_down(TreeNode(1,NullNode,NullNode)))
    println(validate_bottom_up(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode))))
    println(validate_top_down(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode))))
    println(validate_bottom_up(TreeNode(1,NullNode,TreeNode(2,TreeNode(1,NullNode,NullNode),NullNode))))
    println(validate_top_down(TreeNode(1,NullNode,TreeNode(2,TreeNode(1,NullNode,NullNode),NullNode))))
    println(validate_bottom_up(TreeNode(1,NullNode,TreeNode(2,TreeNode(3,NullNode,NullNode),NullNode))))
    println(validate_top_down(TreeNode(1,NullNode,TreeNode(2,TreeNode(3,NullNode,NullNode),NullNode))))
    println(validate_bottom_up(TreeNode(1,NullNode,TreeNode(4,TreeNode(3,TreeNode(2,NullNode,NullNode),NullNode),NullNode))))
    println(validate_top_down(TreeNode(1,NullNode,TreeNode(4,TreeNode(3,TreeNode(2,NullNode,NullNode),NullNode),NullNode))))
  }
}
