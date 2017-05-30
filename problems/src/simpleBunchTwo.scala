/**
  * Created by syncshan on 01/05/2017.
  */
class simpleBunchTwo {
  //Q14
  def duplicate(ls:List[Any]) : List[Any]  = ls match{
    case x::tail => x::x::duplicate(tail)
    case nil => List()
  }
  //Q14 Ans
  def duplicateAns[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }

  //Q15
  def duplicateN[A](n:Int, ls:List[A]) = ls flatMap { e => List.fill(n)(e)}

  //Q16
  def drop[A](n:Int, ls:List[A]) : List[A] = {
    def recDrop[A](n:Int,ls:List[A],res:List[A]):List[A] = ls match{
      case x::tail if(n == 1) => res:::tail
      case x::tail if(n > 1) => recDrop(n-1,tail,res:::List(x))
      case x::tail if(n<=0) => throw new NoSuchElementException
    }
    recDrop(n,ls,List())
  }

  //Q16 ans
  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => result.reverse
      case (1, _ :: tail) => dropR(n, tail, result)
      case (_, h :: tail) => dropR(c - 1, tail, h :: result)
    }
    dropR(n, ls, Nil)
  }
  // cool zipWithIndex
  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  //Q17
  def split[A](n:Int, ls:List[A]):(List[A],List[A]) = ls splitAt(n)

  def split2[A](n:Int, ls:List[A]):(List[A], List[A]) = {
    def splitRec(n:Int, ls:List[A],res:List[A]):(List[A],List[A]) = (n,ls) match {
      case(_, Nil) => (res.reverse,List())
      case(1,h::tail) => ((h::res).reverse,tail)
      case(_,h::tail) => splitRec(n-1,tail,h::res)
    }
    splitRec(n,ls,List())
  }
  //Q17 ans
  def splitFunctional[A](n: Int, ls: List[A]): (List[A], List[A]) =
    (ls.take(n), ls.drop(n))

  //Q18
  def slice[A](s:Int,e:Int,ls:List[A]):  List[A] = ls.take(e).drop(s)

  //Q19
  def rotate[A](n:Int,ls:List[A]) : List[Any] =  {
    if(n > 0) return rotate(n-1,ls.tail:::List(ls.head))
    if(n < 0) return rotate(n+1,ls.reverse.head::ls.reverse.tail.reverse)
    ls
  }

  //Q19 Ans
  def rotateAns[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotateAns(nBounded + ls.length, ls)
    else (ls drop nBounded) ::: (ls take nBounded)
  }

  //Q20
  def removeAt[A](n:Int, ls:List[A]) = (ls.zipWithIndex.filter(_._2!=n).map(_._1),ls(n))

  //Q20
  def removeAtAns[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
  }

  def removeAt2[A](n: Int, ls: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else (n, ls) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t, e) = removeAt(n - 1, ls.tail)
        (ls.head :: t, e)
      }
    }

  //Q21
  def insertAt[A](x:A,n:Int,ls:List[A]) = ls.splitAt(n) match{
    case (h,t) => h:::(x::t)
  }

  //Q22
  def range(s:Int,e:Int): List[Int] = {
    def rangeRec(start:List[Int],n:Int,target:Int) : List[Int] = {
      if(n == target) (n::start).reverse
      else rangeRec(n::start,n+1,target)
    }
    rangeRec(List(),s,e)
  }

  //Q22 ans
  def rangeRecursive(start: Int, end: Int): List[Int] =
    if (end < start) Nil
    else start :: rangeRecursive(start + 1, end)
  // The classic functional approach would be to use `unfoldr`, which Scala
  // doesn't have.  So we'll write one and then use it.
  def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
    f(s) match {
      case None         => Nil
      case Some((r, n)) => r :: unfoldRight(n)(f)
    }
  def rangeFunctional(start: Int, end: Int): List[Int] =
    unfoldRight(start) { n =>
      if (n > end) None
      else Some((n, n + 1))
    }


}
