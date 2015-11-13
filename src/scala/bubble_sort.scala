package scala

/**
 * Created by luozhenjun on 15/11/13.
 */
//怎么交换，从前到后交换和从后到前交换，实现上面有什么不同？
//从前到后，把最大的交换到最底部
//从后到前，把最小的交换到最前面
object bubble_sort {

  def bubble_sort_loop(xs:Array[Int]):Array[Int] = {
    val len = xs.length-1;
    for(i <- 1 to len){
      for(j <- len to i by -1){
        if(xs(j) < xs(j-1)){
          val tmp = xs(j)
          xs(j) = xs(j-1)
          xs(j-1) = tmp
        }
      }
    }
    xs
  }

  def bubble_sort_loop2(xs:Array[Int]):Array[Int] = {
    val len = xs.length-1
    var swaped = false
    for(i <- 1 to len){
      for (j <- len to i by -1) {
        if (xs(j) < xs(j - 1)) {
          val tmp = xs(j)
          xs(j) = xs(j - 1)
          xs(j - 1) = tmp
          swaped = true
        }
      }
      if(!swaped)return xs
    }
    xs
  }

  def bubble_sort(xs:List[Int]):List[Int] = {
    def bubble(xs:List[Int]):List[Int] = xs match {
      case Nil => Nil
      case a :: Nil => List(a)
      case a :: b :: tail if a > b => b::bubble(a::tail)
      case a :: b :: tail => a :: bubble(b::tail)
    }
    def sort(x:List[Int]):List[Int] = {
      val b = bubble(x)
      b.head :: bubble(b.tail)
    }
    sort(xs)
  }

}
