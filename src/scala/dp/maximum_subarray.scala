package scala.dp

/**
 * Created by lzj on 15-11-27.
 */

/*
Maximum Subarray
描述
Find the contiguous subarray within an array (containing at least one number) which has the largest sum.
For example, given the array [−2,1,−3,4,−1,2,1,−5,4], the contiguous subarray [4,−1,2,1] has
the largest sum = 6.
*/


object maximum_subarray {
  def max_subarr(xs: List[Int]): Int = {
    var max = 0
    var end = 0
    for(e <- xs){
      end = List(e+end,e,0).max
      max = utils.max(end,max)
    }
    return max
  }

  def run(): Unit ={
    println(max_subarr(List(-2,1,-3,4,-1,2,1,-5,4)))
    println(max_subarr(List(-2,1,-3)))
    println(max_subarr(List(1)))
    println(max_subarr(List(-1,-2)))
    println(max_subarr(List(1,2,3,4)))
    println(max_subarr(List(-1,-2,-3,1)))
    println(max_subarr(List(-1,-2,-3,1,2)))
  }
}
