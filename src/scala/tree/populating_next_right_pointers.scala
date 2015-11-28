package scala.tree

import scala.ADT.{TreeNodeWithNext, TreeNode, NullNode, Tree}

/**
 * Created by lzj on 15-11-27.
 */

/*

Given a binary tree
struct TreeLinkNode {
  int val;
  TreeLinkNode *left, *right, *next;
  TreeLinkNode(int x) : val(x), left(NULL), right(NULL), next(NULL) {}
};

Populate each next pointer to point to its next right node. If there is no next right node, the next pointer
should be set to NULL.
Initially, all next pointers are set to NULL.
Note:
• You may only use constant extra space.
• You may assume that it is a perfect binary tree (ie, all leaves are at the same level, and every parent
has two children).

For example, Given the following binary tree,
                  1
                /  \
               2    3
              / \   / \
             4   5 6   7
After calling your function, the tree should look like:
                    1  -> NULL
                /       \
               2   ->    3 -> NUll
              /  \     /   \
             4 -> 5 -> 6 -> 7 -> Null

*/

//perfect binary tree
object populating_next_right_pointers {

  def populating_next(tree: Tree[Int]): Tree[Int] = {

    def copy(t: Tree[Int],next: Tree[Int]): Tree[Int] = t match {
      case NullNode => NullNode
      case TreeNode(v,l,r) =>
        //这样做会有缺陷，每一层没有连接起来,有些node没有next节点
        next match {
          case NullNode => TreeNodeWithNext(v, copy(l,r),copy(r,NullNode),next)
          case TreeNode(_,ll, rr) => TreeNodeWithNext(v, copy(l,r),copy(r,ll),next)
        }
    }
    copy(tree,NullNode)
  }

  def populating_next_ii(tree: Tree[Int]): Tree[Int] = {

    def copy(t: Tree[Int], next: List[Tree[Int]]): Tree[Int] = t match {
      case NullNode => NullNode
      case TreeNode(v,l,r) =>
        next match {
          case List(NullNode) => TreeNodeWithNext(v, copy(l,List(r)),copy(r,List(NullNode)),NullNode)
          case _ =>
            val nnext = next.flatMap{ node =>
              node match {
                case NullNode => Nil
                case TreeNode(_,nl,nr) => List(nl,nr)
              }
            }
            if(next.size > 1){
              val root::nnnext = next
              TreeNodeWithNext(v, copy(l,r::nnext),copy(r,nnext),copy(root,nnnext))
            }else{
              TreeNodeWithNext(v, copy(l,r::nnext),copy(r,nnext),copy(next.head,List(NullNode)))
            }

        }
    }

    copy(tree,List(NullNode))
  }


  def run(): Unit ={
    println(populating_next(TreeNode(1,NullNode,NullNode)))
    println(populating_next(TreeNode(1,TreeNode(2,NullNode,NullNode),TreeNode(3,NullNode,NullNode))))
    println(populating_next(TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(4,NullNode,NullNode),
        TreeNode(5,NullNode,NullNode)),
      TreeNode(
        3,
        TreeNode(6,NullNode,NullNode),
        TreeNode(7,NullNode,NullNode)))))

    TreeNodeWithNext(
      1,
      TreeNodeWithNext(
        2,
        TreeNodeWithNext(4,
          NullNode,NullNode,
          TreeNode(5,NullNode,NullNode)),
        TreeNodeWithNext(5,
          NullNode,NullNode,
          //这个node没有next
          TreeNode(6,NullNode,NullNode)),
        TreeNode(3,
          TreeNode(6,NullNode,NullNode),TreeNode(7,NullNode,NullNode))),
      TreeNodeWithNext(
        3,
        TreeNodeWithNext(6,
          NullNode,NullNode,TreeNode(7,NullNode,NullNode)),
        TreeNodeWithNext(7,
          NullNode,NullNode,NullNode),
        NullNode),
      NullNode)


    println(populating_next_ii(TreeNode(
      1,
      TreeNode(
        2,
        TreeNode(4,NullNode,NullNode),
        TreeNode(5,NullNode,NullNode)),
      TreeNode(
        3,
        TreeNode(6,NullNode,NullNode),
        TreeNode(7,NullNode,NullNode)))))


    TreeNodeWithNext(
      1,
      TreeNodeWithNext(
        2,
        TreeNodeWithNext(
          4,
          NullNode,
          NullNode,
          TreeNodeWithNext(
            5,
            NullNode,
            NullNode,
            TreeNodeWithNext(
              6,
              NullNode,
              NullNode,
              TreeNodeWithNext(
                7,
                NullNode,
                NullNode,
                NullNode)))),
        TreeNodeWithNext(
          5,
          NullNode,
          NullNode,
          TreeNodeWithNext(
            6,
            NullNode,
            NullNode,
            TreeNodeWithNext(
              7,
              NullNode,
              NullNode,
              NullNode))),
        TreeNodeWithNext(
          3,
          TreeNodeWithNext(
            6,
            NullNode,
            NullNode,
            TreeNodeWithNext(
              7,
              NullNode,
              NullNode,
              NullNode)),
          TreeNodeWithNext(
            7,
            NullNode,
            NullNode,
            NullNode),
          NullNode)),
      TreeNodeWithNext(
        3,
        TreeNodeWithNext(
          6,
          NullNode,
          NullNode,
          TreeNodeWithNext(
            7,
            NullNode,
            NullNode,
            NullNode)),
        TreeNodeWithNext(
          7,
          NullNode,
          NullNode,
          NullNode),
        NullNode),
      NullNode)


  }
}
