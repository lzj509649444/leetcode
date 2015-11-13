package scala

/**
 * Created by luozhenjun on 15/11/13.
 */
//怎么交换，从前到后交换和从后到前交换，实现上面有什么不同？
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

}
