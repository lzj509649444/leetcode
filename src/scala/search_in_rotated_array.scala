package scala

/**
 * Created by lzj on 15-11-9.
 */

/*Search in Rotated Sorted Array
描述
Suppose a sorted array is rotated at some pivot unknown to you beforehand.2.1 数组
(i.e., 0 1 2 4 5 6 7 might become 4 5 6 7 0 1 2).
You are given a target value to search. If found in the array return its index, otherwise return -1.
You may assume no duplicate exists in the array.*/

//开区间，闭区间要想清楚

object search_in_rotated_array {

  def search_in_roatated(xs:List[Int],target:Int):Option[Int]={
    def fun(xs:List[Int], s:Int, e:Int): Option[Int] ={
      if (s > e) return None;
      val mid = s + (e-s)/2;
      xs(mid) match {
        case t if  t == target => Some(t)
        case b if b < xs(e) =>
          if (b < target && target <= xs(e)){
            fun(xs,mid+1,e)
          }else{
            fun(xs,s,mid-1)
          }
        case c =>
          if(xs(s) <= target && target < c){
            fun(xs,s,mid-1)
          }else{
            fun(xs,mid+1,e)
          }
      }
    }
    fun(xs,0,xs.length-1)
  }

}
