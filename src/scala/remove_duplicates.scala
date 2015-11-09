package scala

/**
 * Created by lzj on 15-11-9.
 */
/*Remove Duplicates from Sorted Array
描述
Given a sorted array, remove the duplicates in place such that each element appear only once
and return the new length.
Do not allocate extra space for another array, you must do this in place with constant memory.
For example, Given input array A = [1,1,2],
Your function should return length = 2, and A is now [1,2].
*/
object remove_duplicates {
  def remove(xs : List[Int]): List[Int] = xs match {
    case Nil => Nil
    case a :: Nil => List(a)
    case a :: b :: tail if a != b => a :: remove(b :: tail)
    case a :: b :: tail if a == b => remove(b :: tail)
  }
}
