package scala

/**
 * Created by lzj on 15-11-14.
 */
//下一个排列
// 1. 由后往前，找到第一个非递增的数，记为partion_number
// 2. 由后往前, 找到第一个大于partion_number的数，记为change_number
// 3. 交换partion_number 和 change_number
// 4. reverse partion_number后面的数
object permutation_next {

  def permutation_next(xs: List[Int]): List[Int] = {
    var partion_number_idx = xs.length-2

    while ( partion_number_idx >= 0 && xs(partion_number_idx) >= xs(partion_number_idx+1)){
      partion_number_idx -= 1
    }

    if(partion_number_idx<0)return xs.reverse

    var change_number_idx = xs.length-1
    while (change_number_idx >=0 && xs(change_number_idx) <= xs(partion_number_idx)){
      change_number_idx -= 1
    }
    if(change_number_idx<0)return xs.reverse

    val nxs = xs.updated(partion_number_idx,xs(change_number_idx))

    val nnxs = nxs.updated(change_number_idx,xs(partion_number_idx))

    nnxs.take(partion_number_idx) :::
      (nnxs(partion_number_idx) :: nnxs.drop(partion_number_idx+1).reverse)
  }

  def run(): Unit ={
    var rrs: List[Int] = List(1,2,3)
    for(i <- 1 to 10){
      rrs = permutation_next(rrs)
      println(rrs)
    }




  }
}
