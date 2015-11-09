package scala

/**
 * Created by lzj on 15-11-9.
 */

/*
Remove Duplicates from Sorted Array II
描述
Follow up for ”Remove Duplicates”: What if duplicates are allowed at most twice?
For example, Given sorted array A = [1,1,1,2,2,3],
Your function should return length = 5, and A is now [1,1,2,2,3]
*/

object remove_duplicates_ii {
  def remove(xs:List[Int]):List[Int] = xs match {
    case Nil => Nil
    case a :: Nil => List(a)
    case a :: b :: Nil => List(a,b)
    case a :: b :: tail if (a != b) => a :: remove(b :: tail)
    case a :: b :: tail if (a == b) =>
      var r = remove(b :: tail)
      r match {
        case Nil => List(a,b)
        case c :: Nil if(b==c) => List(a,b)
        case c :: Nil if(b!=c) => List(a,b,c)
        case c :: d :: tail if (c==d) =>
          if(b==c){
            r
          }else{
            List(a,b) ::: r
          }
        case c :: d :: tail if (c!=d) =>
          if(b==c){
            b :: r
          }else{
            List(a,b) ::: r
          }
      }
  }
}