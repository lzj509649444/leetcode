package scala

/**
 * Created by lzj on 15-11-14.
 */
object quick_sort {
  def qsort[T <% Ordered[T]](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case h :: tail =>
      val (before,after) = tail.partition(_ < h)
      qsort(before) ++ (h::qsort(after))
  }

  def qsort2[T](xs:List[T])(implicit ord: Ordering[T]): List[T] = xs match {
    case Nil => Nil
    case h :: tail =>
      val (before,after) = tail.partition(ord.lt(_,h))
      qsort2(before) ++ (h::qsort2(after))
  }
}
