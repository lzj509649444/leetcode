import scala.tree.{construct_btree_from_preorder_and_inorder, construct_btree_from_inorder_and_postorder}

/**
 * Created by lzj on 15-11-9.
 */
object Main {
  def main(args: Array[String]): Unit = {
    construct_btree_from_inorder_and_postorder.run()
    println()
    construct_btree_from_preorder_and_inorder.run()
  }
}
