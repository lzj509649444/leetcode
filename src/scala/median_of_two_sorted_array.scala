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

//注意区间问题

object median_of_two_sorted_array {

  //闭区间和开区间怎么写?
  def median_of_two(f:List[Int],l:List[Int]): Int = {
    def min(a:Int,b:Int): Int ={
      if(a<b) a else b
    }
    def run(ff:List[Int],m:Int, s:Int,ll:List[Int],n:Int, ss:Int,k:Int): Int = {
      if(ff.length>ll.length)run(ll,n,ss,ff,m,s,k)
      if(m==0)return ll(k-1)
      if(k==1)return min(ff(s),ll(ss))


      val ia = min(k/2,m)
      val ib = k - ia;
      if(ff(s+ia-1) < ll(ss+ib-1)){
        run(ff,m-ia,s+ia,ll,n,ss,k-ia)
      }else if(ff(s+ia-1) > ll(ss+ib-1)){
        run(ff,m,s,ll,n-ib,ss+ib,k-ib)
      }else{
        ff(s+ia-1)
      }
    }

    run(f,f.length,0,l,l.length,0,(f.length+l.length)/2)

  }
}