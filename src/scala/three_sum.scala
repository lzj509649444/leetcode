package scala

import scala.collection.mutable.ListBuffer

/**
 * Created by lzj on 15-11-14.
 */

/*3Sum
描述
Given an array S of n integers, are there elements a, b, c in S such that a + b + c = 0? Find all unique
triplets in the array which gives the sum of zero.
Note:
• Elements in a triplet (a, b, c) must be in non-descending order. (ie, a ≤ b ≤ c)
• The solution set must not contain duplicate triplets.
For example, given array S = {-1 0 1 2 -1 -4}.
A solution set is:
(-1, 0, 1)
(-1, -1, 2)*/

//注意去重复
object three_sum {

  def three_sum(xs: List[Int],t: Int): ListBuffer[(Int,Int,Int)] = {
    val rs = xs.sorted
    var result = ListBuffer[(Int,Int,Int)]()
    for(i <- 0 to rs.length-2){
      var j = i + 1
      if(i>0 && rs(i) == rs(i-1)) i else{
        var k = rs.length-1
        while (j<k){
          if(rs(i)+rs(j)+rs(k) == t){
            result += ((rs(i),rs(j),rs(k)))
            j += 1
            k -= 1
            while ( j < k && rs(j)==rs(j-1) && rs(k) == rs(k+1)) j += 1
          }else if(rs(i)+rs(j)+rs(k) < t){
            j += 1
            while ( j < k && rs(j) == rs(j-1)) j += 1
          }else{
            k -= 1
            while (k > j && rs(k) == rs(k+1)) k -= 1

          }
        }
      }
    }
    result
  }

  def run(): Unit ={
    println(three_sum(List(1,2,3,4,1,2,3,5,6,7,8,9,10,11,12),10))
    println(three_sum(Range(1,20).toList,18))
  }
}
