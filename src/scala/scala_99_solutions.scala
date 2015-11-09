package scala

/**
 * Created by lzj on 15-11-9.
 */
object scala_99_solutions {
  // P01: Find the last element of a list.

  def mlast[A](xs: List[A]) = xs.last

  def mlast2[A](xs: List[A]): Option[A] = {
    xs match {
      case head::Nil => Some(head)
      case head::tail => mlast2(tail)
      case Nil => None
    }
  }

  def mlast3[A](l:List[A]):A = l match {
    case h :: Nil => h
    case _ :: tail => mlast3(tail)
    case _ => throw new NoSuchElementException
  }

  // P02: Find the last but one element of a list.

  def lastbutone(xs: List[Int]): Int = xs match{
    case x :: y :: Nil => x
    case x :: y :: tail => lastbutone(y::tail)
    case _ => throw new Exception
  }

  //P03: Find the Kth element of a list.
  def kth(k: Int,xs: List[Int]): Int ={
    var l =  xs.zipWithIndex
    var ll = for((a,i) <- l if i < k) yield a
    mlast3(ll)
  }

  def kth2(k: Int,xs: List[Int]): Int = k match {
    case 0 => xs.head
    case x if x < 0 || x > xs.length - 1 => throw new Exception
    case _ => kth2(k-1,xs.tail)
  }


  // P04: Find the number of elements of a list.
  def findn(n: Int,xs: List[Int]): Boolean = {
    for(a <- xs if a == n) return true
    false
  }

  def len(xs: List[Int]): Int = xs match {
    case Nil => 0
    case h :: tail => 1 + len(tail)
  }

  def len2(xs: List[Int]) = {
    def lenn(n: Int,xs: List[Int]) : Int = xs match {
      case Nil => n
      case h :: tail => lenn(n+1,tail)
    }
    lenn(0,xs)
  }

  // P05: Reverse a list.
  def reverse(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case h :: tail => reverse(tail) :+ h
  }

  def reverse2(xs: List[Int]): List[Int] = xs match{
    case Nil => Nil
    case h :: Nil => List(h)
    case h :: tail => reverse2(tail) ::: List(h)
  }

  // P06: Find out whether a list is a palindrome.
  def isPalindrome(xs: List[Int]): Boolean = xs match {
    case Nil => true
    case h :: Nil => true
    case _ => xs.head == xs.last && isPalindrome(xs.tail.reverse.tail.reverse)
  }

  def isPalindrome2(xs: List[Int]): Boolean = {
    def loop(b:Int,e:Int,xs:List[Int]): Boolean = {
      if(b >= e)return true
      xs(b) == xs(e) && loop(b+1,e-1,xs)
    }

    loop(0,xs.length -1,xs)
  }

  // P07: Flatten a nested list structure.

  //注意参数,case顺序
  def flatten(xs: Any): List[Any] = xs match {
    case Nil => Nil
    case h :: tail => flatten(h) ::: flatten(tail)
    case q => List(q)

  }

  // error
  /*  def flatten2(xs: List[Any]) : List[Any] = xs match {
      case Nil => Nil
      case p => List(p)
      case h :: tail => flatten2(List(h)) ::: flatten2(tail)
    }*/

  // P08: Eliminate consecutive duplicates of list elements.
  //scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  def compress(xs: List[Char]): List[Char] = xs match {
    case Nil => Nil
    case p :: Nil  => List(p)
    case f :: s :: tail =>
      if(f == s)
        compress(s :: tail)
      else
        f :: compress(s :: tail)
  }

  // P09: Pack consecutive duplicates of list elements into sublists.
  /* scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c),
    List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  */
  def pack[T](list: List[T]) : List[List[T]] = list match {
    case Nil => Nil
    case head :: Nil => List(List(head))
    case a :: b :: xs if a != b =>
      List(List(a)) ::: pack(b :: xs)
    case a :: b :: xs =>
      pack(xs) match {
        case Nil => List(List(a,b))
        case restList :: restXs if (restList.head == a) =>
          List(List(a,b) ::: restList) ::: restXs
        case rest => List(List(a,b)) ::: rest
      }
  }

  def pack2(xs: List[Int]): List[List[Int]] = xs match {
    case Nil => Nil
    case a :: Nil => (List(List(a)))
    case a :: b :: tail if a != b => List(List(a)) ::: pack2(b::tail)
    case a :: b :: tail  => pack2(tail) match {
      case c :: d if(c.head == a) =>
        List(a :: b :: c) ::: d
      case r => List(List(a,b)) ::: r

    }
  }

  // P10: Run-length encoding of a list.
  /*scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))*/
  def encode(xs: List[Int]): List[Any] = {
    var res = List[Any]()
    def loop(xs: List[Int]){
      var pre = xs(0)
      var ct = 0
      for(i <- xs){
        if(i == pre){
          ct += 1
        }else{
          res = res ::: List(List(ct,pre))
          pre = i
          ct = 1
        }
      }
    }

    loop(xs)
    res
  }

  def encode2(xs: List[Int]): List[Any] = {
    def recur(xs: List[Int],n: Int): List[Any] = xs match {
      case Nil => Nil
      case h :: Nil => List(List(n+1,h))
      case a :: b :: tail if a == b => recur(b :: tail,n+1)
      case a :: b :: tail if a != b => recur(List(a),n) ::: recur(b :: tail,0)
    }
    recur(xs,0)
  }

  // P11: Modified run-length encoding.
  /*scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  */
  def encodeModified(xs: List[Int]): List[Any] = {
    def recur(xs: List[Int],n: Int): List[Any] = xs match {
      case Nil => Nil
      case h :: Nil if n == 0 => List(h)
      case h :: Nil => List(List(n+1,h))
      case a :: b :: tail if a == b => recur(b :: tail,n+1)
      case a :: b :: tail if a != b => recur(List(a),n) ::: recur(b :: tail,0)
    }
    recur(xs,0)
  }

  //P12: Decode a run-length encoded list.
  /*scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)*/

  def decode[T](xs: List[(Int,T)]): List[T] = {
    xs flatMap { tuple =>
      var (a,b) = tuple
      (1 to a).map(_ => b)
    }
  }

  def decode2[T](xs: List[(Int,T)]): List[T] = {
    var c = for((a,b) <- xs) yield(1 to a map  (_ => b) )
    c.flatMap(a => a)
  }

  // P13: Run-length encoding of a list (direct solution).
  /*scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)*/

  // P14: Duplicate the elements of a list.
  /*scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)*/

  def duplicate[T](xs: List[T]): List[T] = xs flatMap(i => List(i,i))

  // P16: Drop every Nth element from a list.
  def dropn(xs: List[Int],k: Int): List[Int] = {
    def recur(xs: List[Int],n: Int): List[Int] = xs match {
      case Nil => Nil
      case a :: b =>
        var r = recur(b,n+1)
        if(n%k == 0) r else a :: r
    }
    recur(xs,1)
  }

  // P17: Split a list into two parts.
  /*scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  */
  def splitn(n: Int,xs: List[Int]): List[Any] = {
    xs match {
      case Nil => Nil
      case a :: Nil => List(a)
      case a :: b if n == 1 => List(List(a)) ::: List(b)
      case a :: b =>
        var r = splitn(n - 1, b)
        r.head match {
          case p :: q => List(a :: p :: q) ::: r.tail
          case p => a :: r
        }
    }
  }

  def group(n: Int,xs: List[Int]): List[List[Int]] = {
    def recur(x: Int,s: List[Int]): List[List[Int]] = s match{
      case Nil => Nil
      case a::Nil => List(List(a))
      case a::b if x == 1 =>
        var r = recur(n,b)
        if(r.isEmpty)return List(List(a))
        List(List(a)) ::: r
      case a::b =>
        var r = recur(x-1,b)
        r match{
          case c::Nil => List(a::c)
          case c::d   => List(a::c) ::: d
        }
    }
    recur(n,xs)

  }

  // P18: Extract a slice from a list.
  /*scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: List[Symbol] = List('d, 'e, 'f, 'g)*/

  def slice(b: Int,e: Int, xs: List[Int]): List[Int] = {
    for((i,index) <- xs.zipWithIndex if index >= b && index <= e) yield i
  }

  // P19: Rotate a list N places to the left.
  /*scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  cala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)*/
  def rotate(n:Int, xs:List[Int]): List[Int] = {
    var xxs = xs.zipWithIndex
    var a:List[Int] = for((i,index)<-xxs if index < n)yield i
    var b:List[Int] = for((i,index)<-xxs if index >= n)yield i
    b ::: a
  }

  // P20: Remove the Kth element from a list
  /*scala> removeAt(1, List('a, 'b, 'c, 'd))
  res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)*/
  def removeAt(n:Int,xs:List[Int]): List[Int] = {
    for((i,index) <- xs.zipWithIndex if index != n) yield i
  }

  // P21: Insert an element at a given position into a list.
  def insertAt(n:Int,e:Int, xs:List[Int]):List[Int] = {
    var xxs = xs.zipWithIndex
    var a:List[Int] = for((i,index)<-xxs if index < n)yield i
    var b:List[Int] = for((i,index)<-xxs if index >= n)yield i
    a ::: List(e) ::: b
  }

  // P22: Create a list containing all integers within a given range.
  /*scala> range(4, 9)
  res0: List[Int] = List(4, 5, 6, 7, 8, 9)*/
  def range(b:Int,e:Int):List[Int] = {
    var c = b to e map {i=>i}
    c.to[List]
  }

  //P23: Extract a given number of randomly selected elements from a list.
  def randSelect(p:Int,xs:List[Int]):List[Int] = {
    var c = 1  to p map { _ => (Math.random() * xs.length).toInt }
    c.to[List]
  }

  // P25: Generate a random permutation of the elements of a list.
  //def randomPermutaion

  // P26: Generate the combinations of K distinct objects chosen from the N elements of a list.
  /*scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...*/
  // (C(N,K),选和不选
  def combination(n:Int,xs:List[Int]):List[List[Int]] = {
    xs match {
      case Nil => Nil
      case head::tail =>
        if (n <=0 || n > xs.length){
          Nil
        }else if (n == 1){
          xs.map(List(_))
        }else{
          combination(n-1,tail).map(head::_) ::: combination(n,tail)
        }
    }
  }

  // P27: Group the elements of a set into disjoint subsets.

  /*scala> group(List(2,3,4), List('a,'b,'c,'d,'e,'f,'g,'h,'i).length
    res162: Int = 1260

  scala> group(List(2,3,4), List('a,'b,'c,'d,'e,'f,'g,'h,'i).foreach(println)
    List(List('a, 'b), List('c, 'd, 'e), List('f, 'g, 'h, 'i))
    List(List('a, 'b), List('c, 'd, 'f), List('e, 'g, 'h, 'i))
    List(List('a, 'b), List('c, 'd, 'g), List('e, 'f, 'h, 'i))
    List(List('a, 'b), List('c, 'd, 'h), List('e, 'f, 'g, 'i))
    List(List('a, 'b), List('c, 'd, 'i), List('e, 'f, 'g, 'h))
    ...
  List(List('h, 'i), List('c, 'f, 'g), List('a, 'b, 'd, 'e))
  List(List('h, 'i), List('d, 'e, 'f), List('a, 'b, 'c, 'g))
  List(List('h, 'i), List('d, 'e, 'g), List('a, 'b, 'c, 'f))
  List(List('h, 'i), List('d, 'f, 'g), List('a, 'b, 'c, 'e))
  List(List('h, 'i), List('e, 'f, 'g), List('a, 'b, 'c, 'd))
*/

  def groupCom(xs:List[Int],xxs:List[Int]) : List[List[List[Int]]] = {
    xs match {
      case Nil => Nil
      case head :: Nil =>
        combination(head,xxs).map(List(_))
      case head :: tail =>
        var r = groupCom(tail,xxs)
        r.flatMap { i =>
          var in = i.flatMap(i => i)
          var d = xxs diff in
          combination(head,d).map{ c => c :: i}
        }
    }
  }

  // P28: Sorting a list of lists according to length of sublists.
  def lsort(xs:List[List[Int]]):List[List[Int]] = {
    xs.sortBy(i=> i.length)
  }

  // P31: Determine whether a given integer number is prime.
  /*scala> 7.isPrime
  res0: Boolean = true
  */
  implicit def intToNumWrapper(i: Int) : S99Int = new S99Int(i)
  class S99Int(val i: Int) {
    def isPrime : Boolean = {
      if (i <= 1)
        false
      else if (i == 2)
        true
      else
        !(2 to (i-1)).exists(x => i % x == 0)
    }
  }

}
