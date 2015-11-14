package scala

/**
 * Created by lzj on 15-11-14.
 */

/*
Given an array and a value, remove all instances of that value in place and return the new length.
The order of elements can be changed. It doesn’t matter what you leave beyond the new length.
*/

object remove_element {
  def remove(xs: List[Int],k: Int): List[Int] = {
    for( e <- xs if e != k) yield e
  }
}
