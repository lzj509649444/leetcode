package java;

/**
 * Created by lzj on 15-11-9.
 */

/*
给定一个二叉树，找出其最小深度。
二叉树的最小深度为根节点到最近叶子节点的距离。
如过左子树为空，右子树不为空，则最小深度为右子树最小深度
*/

import java.lang.Integer;
import java.lang.Math;

public class minimum_depth_of_binary_tree {

    //Definition of TreeNode:
    class TreeNode {
        public int val;
        public TreeNode left, right;

        public TreeNode(int val) {
            this.val = val;
            this.left = this.right = null;
        }
    }

    class Solution1 {
        /**
         * @param root: The root of binary tree.
         * @return: An integer.
         */
        public int minDepth(TreeNode root) {
            // write your code here
            return minD(root, null, 0);
        }

        public int minD(TreeNode root, TreeNode t, int n) {
            if (root == null) {
                if (t == null) {
                    return n;
                } else {
                    return Integer.MAX_VALUE;
                }
            }
            int l = minD(root.left, root.right, n + 1);
            int r = minD(root.right, root.left, n + 1);
            return l > r ? r : l;

        }
    }

    class Solution2 {
        /**
         * @param root: The root of binary tree.
         * @return: An integer.
         */
        public int minDepth(TreeNode root) {
            // write your code here
            if (root == null) return 0;
            return minD(root);
        }

        public int minD(TreeNode root) {
            if (root == null) return Integer.MAX_VALUE;
            if (root.left == null && root.right == null) return 1;
            return Math.min(minD(root.left), minD(root.right)) + 1;
        }
    }


    class Solution3 {
        /**
         * @param root: The root of binary tree.
         * @return: An integer.
         */
        public int minDepth(TreeNode root) {
            // write your code here
            return minD(root, false);
        }

        public int minD(TreeNode root, boolean has_sibling) {
            if (root == null) return has_sibling ? Integer.MAX_VALUE : 0;
            return Math.min(minD(root.left, root.right != null), minD(root.right, root.left != null)) + 1;
        }
    }
}