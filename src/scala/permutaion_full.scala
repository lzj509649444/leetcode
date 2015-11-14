package scala

/**
 * Created by lzj on 15-11-14.
 */
//1-n的全排列
//不需要按照字典顺序大小输出
//枚举每一个位置
object permutaion_full {

  def permutaion(n: Int): Unit = {
    var tmp = new Array[Int](n)
    def runner(cur: Int): Unit = {
      if(cur >= n){
        for(e <- tmp)print(e)
        println()
      }else{
        for(i <- 1 to n){
          var used = false
          for(j <- 0 to cur-1 if tmp(j) == i)used=true
          if(!used){
            tmp.update(cur,i)
            runner(cur+1)
          }
        }
      }
    }
    runner(0)
  }


  def run(): Unit ={
    permutaion(3)
  }
}
