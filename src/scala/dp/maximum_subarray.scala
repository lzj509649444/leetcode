package scala.dp

/**
 * Created by lzj on 15-11-27.
 */
object maximum_subarray {
  def max_subarr(xs: List[Int]): Int = {
    var max = 0
    var end = 0
    for(e <- xs){
      end = List(e+end,e,0).max
      max = utils.max(end,max)
    }
    return max
  }

  def run(): Unit ={
    println(max_subarr(List(-2,1,-3,4,-1,2,1,-5,4)))
  }
}
