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
  def two_sum(x:List[Int],k:Int): (Int,Int) = {

    def run(xs:List[(Int,Int)],s:Int,e:Int): (Int,Int) = (s,e) match {
      case a if (a._1 >= a._2) => (-1,-1)
      case b if ((xs(b._1)._1+xs(b._2)._1)==k) =>
        val p1 = xs(b._1)._2
        val p2 = xs(b._2)._2
        val pp = if(p1>p2) (p2,p1) else (p1,p2)
        pp
      case c if ((xs(c._1)._1+xs(c._2)._1)<k) => run(xs,s+1,e)
      case c if ((xs(c._1)._1+xs(c._2)._1)>k) => run(xs,s,e-1)
    }

    run(x.zipWithIndex.sortBy(_._1),0,x.length-1)
  }

  def run(): Unit ={
    println(two_sum(List(1,2,3,5),5))
    println(two_sum(List(1,2,3,5),6))
  }
}
