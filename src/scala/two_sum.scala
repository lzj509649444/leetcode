package scala

/**
 * Created by lzj on 15-11-12.
 */

/*Two Sum
描述
Given an array of integers, find two numbers such that they add up to a specific target number.
The function twoSum should return indices of the two numbers such that they add up to the target, where
index1 must be less than index2. Please note that your returned answers (both index1 and index2) are not
zero-based.
You may assume that each input would have exactly one solution.
Input: numbers={2, 7, 11, 15}, target=9
Output: index1=1, index2=2*/
object two_sum {
  def two_sum(xs:List[Int],k:Int): List[(Int,Int)] = {
    def run(s:Int,e:Int): List[(Int,Int)] = (s,e) match {
      case a if (a._1 >= a._2) => Nil
      case b if ((xs(b._1)+xs(b._2))==k) => (b._1,b._2) :: run(s+1,e-1)
      case c if ((xs(c._1)+xs(c._2))<k) => run(s+1,e)
      case c if ((xs(c._1)+xs(c._2))>k) => run(s,e-1)
    }

    run(0,xs.length-1)
  }
}
