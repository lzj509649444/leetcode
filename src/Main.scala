import scala.tree.{postorder_traversal_btree,inorder_traversal_btree,preorder_traversal_btree}

/**
 * Created by lzj on 15-11-9.
 */
object Main {
  def main(args: Array[String]): Unit = {
    postorder_traversal_btree.run()
    inorder_traversal_btree.run()
    preorder_traversal_btree.run()
  }
}
