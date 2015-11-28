package scala.tree

import scala.ADT.{TreeNodeWithNext, TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-27.
 */

//could be any binary tree?

/*

Follow up for problem ”Populating Next Right Pointers in Each Node”.
What if the given tree could be any binary tree? Would your previous solution still work?
Note: You may only use constant extra space.
For example, Given the following binary tree,
                 1
                / \
               2   3
              / \   \
             4   5   7
After calling your function, the tree should look like:
                  1  -> NULL
                /   \
               2  -> 3 -> NUll
              /  \    \
             4 -> 5 -> 7 -> Null

*/

object populating_next_right_pointers_ii {

  def populating_ii(tree: Tree[Int]): Tree[Int] = {

    def copy(t: Tree[Int],next: List[Tree[Int]]): Tree[Int] = t match {
      case NullNode => NullNode
      case TreeNode(v,l,r) =>
        next match {
          case List(NullNode) =>
            l match {
              case NullNode => TreeNodeWithNext(v, NullNode,copy(r,List(NullNode)),NullNode)
              case _ => TreeNodeWithNext(v, copy(l,List(r)),copy(r,List(NullNode)),NullNode)
            }
          case _ =>
            val nnext = next.flatMap{ node =>
              node match {
                case NullNode => Nil
                case TreeNode(_,nl,nr) => List(nl,nr)
              }
            }
            if(next.size > 1){
              val root::nnnext = next
              r match {
                case NullNode => TreeNodeWithNext(v, copy(l,nnext),NullNode,copy(root,nnnext))
                case _ => TreeNodeWithNext(v, copy(l,r::nnext),copy(r,nnext),copy(root,nnnext))
              }

            }else{
              r match {
                case NullNode => TreeNodeWithNext(v, copy(l,nnext),NullNode,copy(next.head,List(NullNode)))
                case _ => TreeNodeWithNext(v, copy(l,r::nnext),copy(r,nnext),copy(next.head,List(NullNode)))
              }
            }
        }
    }
    copy(tree,List(NullNode))
  }

  def run(): Unit ={
    println(populating_ii(TreeNode(1,NullNode,NullNode)))
    println(populating_ii(TreeNode(1,TreeNode(2,NullNode,NullNode),NullNode)))
    println(populating_ii(TreeNode(1,NullNode,TreeNode(2,NullNode,NullNode))))
    println(populating_ii(TreeNode(1,TreeNode(3,NullNode,NullNode),TreeNode(2,NullNode,NullNode))))
    println(populating_ii(TreeNode(1,TreeNode(2,TreeNode(5,NullNode,NullNode),NullNode),TreeNode(3,NullNode,TreeNode(4,NullNode,NullNode)))))
    println(populating_ii(TreeNode(1,TreeNode(2,NullNode,TreeNode(5,NullNode,NullNode)),TreeNode(3,NullNode,TreeNode(4,NullNode,NullNode)))))

    println(populating_next_right_pointers.populating_next_ii(TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(4,NullNode,NullNode),
        TreeNode(5,NullNode,NullNode)),
      TreeNode(
        3,
        TreeNode(6,NullNode,NullNode),
        TreeNode(7,NullNode,NullNode)))))

    println(populating_ii(TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(4,NullNode,NullNode),
        TreeNode(5,NullNode,NullNode)),
      TreeNode(
        3,
        TreeNode(6,NullNode,NullNode),
        TreeNode(7,NullNode,NullNode)))))

  }
}
