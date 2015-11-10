package scala

/**
 * Created by lzj on 15-11-9.
 */
/*
Search in Rotated Sorted Array II
描述
Follow up for ”Search in Rotated Sorted Array”: What if duplicates are allowed?
Would this affect the run-time complexity? How and why?
Write a function to determine if a given target is in the array.
允许重复
*/

//开区间，闭区间要想清楚

object search_in_rotated_array_ii {
  def search_in_roatated_ii(xs:List[Int],target:Int):Option[Int]={
    def fun(xs:List[Int], s:Int, e:Int): Option[Int] ={
      if (s > e) return None;
      val mid = s + (e-s)/2;
      xs(mid) match {
        case t if  t == target => Some(t)
        case b if b < xs(e) =>
          if (b < target && target <= xs(e)){
            fun(xs,mid+1,e)
          }else{
            fun(xs,s,mid)
          }
        case c if c == xs(e) =>
          if(xs(s)==c){
            fun(xs,s+1,e)
          }else if (target < c){
            fun(xs,mid+1,e-1)
          }else{
            fun(xs,s,mid-1)
          }
        case d if d > xs(e) =>
          if(xs(s)<=target && target < d){
            fun(xs,s,mid-1)
          }else{
            fun(xs,mid+1,e)
          }
      }
    }
    fun(xs,0,xs.length-1)
  }
}
