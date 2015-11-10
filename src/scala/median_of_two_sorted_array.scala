package scala

/**
 * Created by lzj on 15-11-10.
 */

/*
Median of Two Sorted Arrays
描述
There are two sorted arrays A and B of size m and n respectively. Find the median of the two sorted
arrays. The overall run time complexity should be O(log(m + n)).
*/

//每次能取出每个数组的一半吗？若不能，每次能去除某个数组的一半吗？
//怎么样才能丢弃总数组的一半?

object median_of_two_sorted_array {
  def median_of_two(f:List[Int],l:List[Int]): Int = {
    def min(a:Int,b:Int): Int ={
      if(a<b){
        a
      }else{
        b
      }
    }
    def run(ff:List[Int],m:Int, s:Int,e:Int,ll:List[Int],n:Int, ss:Int,ee:Int,len:Int): Int = {
      if(m==0)return ll(len)
      if(len==1)return min(ff(s),ll(s))

      val ia = min(len/2,m)
      val ib = len - ia;
    }
  }
}
