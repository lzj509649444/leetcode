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

  //从前往后沉下去
  def bubble_sort(xs:List[Int]):List[Int] = {
    def bubble(xs:List[Int]):List[Int] = xs match {
      case Nil => Nil
      case a :: Nil => List(a)
      case a :: b :: tail if a < b => b::bubble(a::tail)
      case a :: b :: tail => a :: bubble(b::tail)
    }
    def sort(x:List[Int]):List[Int] = x match {
      case Nil => Nil
      case _ =>
        val b = bubble(x)
        val c = b.dropRight(1)
        b.last :: sort(c)
    }
    sort(xs)
  }

  //从后往前浮上来
  def bubble_sort2(xs:List[Int]):List[Int] = {
    def bubble(xs:List[Int]):List[Int] = xs match {
      case Nil => Nil
      case a :: Nil => List(a)
      case a :: b =>
        val rr = bubble(b)
        rr match {
          case Nil => List(a)
          case c :: d => if(c<a) c::a::d else a :: rr
        }
    }
    def sort(x:List[Int]):List[Int] = {
      val r = bubble(x)
      if(r.isEmpty)return Nil
      r.head :: sort(r.drop(1))
    }
    sort(xs)
  }

  def run(): Unit ={
    println(bubble_sort2(List(1,2,4,1,3,1,6,5)).foreach(print))
    println(bubble_sort2(List(1,2,1,2,1)).foreach(print))
    println(bubble_sort2(List(1,2,1)).foreach(print))
    println(bubble_sort2(List(1)).foreach(print))
    println(bubble_sort2(List(2,1)).foreach(print))
    println(bubble_sort2(List(6,5,4,3,2,1)).foreach(print))
  }

}
