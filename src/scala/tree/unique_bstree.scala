package scala.tree

/**
 * Created by lzj on 15-11-28.
 */
//递归，k为root的数量,f(k) = f(k-1) * f(n-k), 两个子问题也要递归的进行求解
//如何避免递归重复计算，动态规划： 子问题的解依赖更小子问题的解，两个变量(n-1,k)
//TODO: 动规为什么需要外循环，而不是一个循环？
// f(n) = ∑ f(k − 1) × f(n − k)


object unique_bstree {

  def unique_bstree(n: Int): Int = n match {
    case 0 => 1
    case 1 => 1
    case _ =>
      var number = 0
      for( k <- 1 to n){
        number += unique_bstree(k-1) * unique_bstree(n-k)
      }
      number
  }

  def dp_unique_bstree(n: Int): Int = {
    if(n<2) return 1
    val arr = new Array[Int](n+1)
    arr(0) = 1
    arr(1) = 1

    //子问题
    for(i <- 2 to n){
      // 计算k作为root的数量
      var num = 0
      for( k <- 1 to i){
        num += arr(k-1) * arr(i-k)
      }
      arr(i) = num
    }
    arr(n)
  }

  def run(): Unit ={
    println(unique_bstree(0))
    println(dp_unique_bstree(0))
    println(unique_bstree(1))
    println(dp_unique_bstree(1))
    println(unique_bstree(2))
    println(dp_unique_bstree(2))
    println(unique_bstree(3))
    println(dp_unique_bstree(3))
    println(unique_bstree(6))
    println(dp_unique_bstree(6))
  }
}
