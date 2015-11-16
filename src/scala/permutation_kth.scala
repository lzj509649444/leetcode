package scala

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Created by lzj on 15-11-14.
 */
//输出第k个排列
//全排列的顺序都知道，第k个排列有什么规律
//前到后推导
//后到前推导
//在纸上把推导过程写一边，在写代码
//明白了，不一定能bug free
object permutation_kth {

  def permutation_kth(n: Int, k: Int): List[Int] ={

    @tailrec
    def factorial(nn: Int, reslt: Int): Int = nn match {
      case 1 => reslt
      case _ => factorial(nn-1,reslt*nn)
    }

    def run(ab: ArrayBuffer[Int],n: Int, fac: Int, kk: Int): List[Int] = n match {
      case a if a == 1 =>
        ab(0) :: Nil
      case _ =>
        val b = kk /  fac
        ab(b) :: run(ab -= ab(b), n-1, fac/(n-1), kk % fac)
    }

    var arr = new ArrayBuffer[Int](n)
    arr ++= Range(1,n+1)
    run(arr, n, factorial(n-1,1), k-1)
  }


  def run(): Unit ={
    println(permutation_kth(3,1))
    println(permutation_kth(3,2))
  }
}
