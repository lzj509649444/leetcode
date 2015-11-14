package scala

/**
 * Created by lzj on 15-11-11.
 */

/*Longest Consecutive Sequence
描述
Given an unsorted array of integers, find the length of the longest consecutive elements sequence.
For example, Given [100, 4, 200, 1, 3, 2], The longest consecutive elements sequence is [1,
2, 3, 4]. Return its length: 4.
Your algorithm should run in O(n) complexity.*/

//第一想法事Hash,怎么优化Hash方法
//对hash的优化 => 并查集

object longest_consecutive_sequence {
  def lcs(xs:List[Int]):Int = {
    var map:Map[Int,(Int,Int)] = Map()
    var max = 1
    val f:(Int,Int) => Unit = { (a,b) =>
      val v = map.getOrElse(b,(0,0))
      val va = map.getOrElse(a,(0,0))
      if(v._2 > 0){
        val vv = map.getOrElse(v._2,(0,0))
        map += (a -> (vv._1+va._1,0))
        map += (b -> (0,a))
      }else{
        map += (a -> (v._1 + va._1,0))
        map += (b -> (0,a))

      }
    }
    for(i <- xs){
      map += (i -> (1,0))

      if(map.contains(i-1)){
        f(i,i-1)
      }
      if(map.contains(i+1)){
        f(i,i+1)
      }

      max = utils.max(max,map.getOrElse(i,(0,0))._1)
      //println(map)
    }
    max
  }

  def run(): Unit ={
    println(lcs(List(2,4,7,3,1)))
    println(lcs(List(1)))
    println(lcs(List(1,2)))
    println(lcs(List(1,2,3,4,5)))
    println(lcs(List(5,4,3,2,1)))
    println(lcs(List(5,4)))
    println(lcs(List(1,2,4,3)))
    println(lcs(List(1,3,5,7)))
  }
}
