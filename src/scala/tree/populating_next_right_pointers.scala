package scala.tree

import scala.ADT.Tree

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
  def populating_next(tree: Tree[Int]): Unit ={

  }
}