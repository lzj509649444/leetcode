package scala.search

/**
 * Created by lzj on 15-12-1.
 */

/*Given a sorted array of integers, find the starting and ending position of a given target value.
Your algorithm’s runtime complexity must be in the order of O(log n).
If the target is not found in the array, return [-1, -1].
For example, Given [5, 7, 7, 8, 8, 10] and target value 8, return [3, 4].*/

//注意开区间，闭区间，否则会造成死循环
//循环的时候，区间要统一，否则会造成死循环

object search_for_range {

  def search(arr: Array[Int],target: Int): (Int,Int) ={
    def lower_bound(arr: Array[Int],b: Int, e:Int, t: Int,r: Int): Int = (b,e) match {
      case (bb,ee) if bb > ee => r
      case (bb,ee) if bb == ee && arr(bb) == t => bb
      case (bb,ee) =>
        val m = bb + (ee-bb)/2
        if (arr(m) == t){
          lower_bound(arr,bb,m-1,t,m)
        }else if(arr(m) > t){
          lower_bound(arr,bb,m-1,t,r)
        }else{
          lower_bound(arr,m+1,ee,t,r)
        }
    }

    def upper_bound(arr: Array[Int],b: Int, e:Int, t: Int,r: Int): Int = (b,e) match {
      case (bb,ee) if bb > ee => r
      case (bb,ee) if bb == ee && arr(bb) == t => bb
      case (bb,ee) =>
        val m = bb + (ee-bb)/2
        if (arr(m) == t){
          upper_bound(arr,m+1,ee,t,m)
        }else if(arr(m) > t){
          upper_bound(arr,bb,m-1,t,r)
        }else{
          upper_bound(arr,m+1,ee,t,r)
        }
    }
    (lower_bound(arr,0,arr.length-1,target,-1),upper_bound(arr,0,arr.length-1,target,-1))
  }

  def run(): Unit ={
    //println(search(Array(1),1))
    //println(search(Array(1),2))
    //println(search(Array(1,2),2))
    println(search(Array(1,2),1))
    println(search(Array(1,1,2),1))
    println(search(Array(1,1,2),2))
    println(search(Array(1,2,2),2))
    println(search(Array(1,1),2))
    println(search(Array(1,1,2,2,3,3,3,4,5),3))

  }

}
