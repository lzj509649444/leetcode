/**
 * Created by lzj on 15-11-9.
 */
object Main {
  def main(args: Array[String]): Unit = {
    println(remove_duplicates_ii.remove(List(1,1,1,1,1,1)))
    println(remove_duplicates_ii.remove(List(1,1,1,1,2,2,2,3)))
    println(remove_duplicates_ii.remove(List(1,1,1,1,2,2,2,3,3,3,4,4,5,5,5,6)))
    println(remove_duplicates_ii.remove(List(1,1)))
    println(remove_duplicates_ii.remove(List(1)))
    println(remove_duplicates_ii.remove(List()))

  }
}
