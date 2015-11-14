package scala

import scala.collection.mutable.ListBuffer

/**
 * Created by lzj on 15-11-14.
 */
object combination {

  //只包含第一个元素的时候有那些情况
  //只包含第二个元素的时候有那些情况
  def combination(
                   xs: List[Int],
                   result: ListBuffer[ListBuffer[Int]],
                   tmp: ListBuffer[Int], k: Int
                   ): Unit ={
    if(k==0){
      result += tmp.clone()
    }else{
      for( (e,index) <- xs.zipWithIndex){
        if(index > 0 && xs(index-1) == xs(index)) e else{
          tmp += e
          combination(xs.drop(index+1),result,tmp,k-1)
          tmp -= e
        }
      }
    }
  }

  //每次选和不选
  def combination2(xs: List[Int], k: Int): List[List[Int]] = xs match {
    case Nil => Nil
    case h :: tail =>
      if(k <= 0 || k > xs.length){
        Nil
      }else if(k==1){
        xs.map(List(_))
      }else{
        combination2(tail,k-1).map(h :: _) ::: combination2(tail,k)
      }
  }

  def run(): Unit ={
    /*var resutl = ListBuffer[ListBuffer[Int]]()
    var tmp = ListBuffer[Int]()
    combination(List(1,1,2,3),resutl,tmp,2)
    println(resutl)*/

    println(combination2(List(1,2,3),2))

  }
}
