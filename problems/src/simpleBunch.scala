import java.util.NoSuchElementException

import scala.collection.concurrent.RDCSS_Descriptor

/**
  * Created by syncshan on 23/04/2017.
  */
class simpleBunch {
  //Q1
  def last(a:List[Int]): Int ={
    if(a.length == 1) a(0)
    else last(a.tail)
  }

  //Q1 answer
  def lastRecursive[A](ls: List[A]): A = ls match {
    case h :: Nil  => h
    case _ :: tail => lastRecursive(tail)
    case _         => throw new NoSuchElementException
  }

  //Q2
  def penultimate[A](ls : List[A]) :A = ls match {
    case h:: Nil    => throw  new NoSuchElementException
    case h::j::Nil  => h
    case h::j::tail => penultimate(j::tail) // answer is better
    case _          => throw new NoSuchElementException

  }

  //Q2 answer
  def penultimateRecursive[A](ls: List[A]): A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail     => penultimateRecursive(tail)
    case _             => throw new NoSuchElementException
  }

  //Q3
  def nth[A]( n : Int, ls : List[A]) : A = ls match {
    case Nil => throw new NoSuchElementException
    case h :: tail if n == 0 => h
    case h :: tail if n > 0 => nth(n-1,tail)
    case h :: tail if n < 0 => throw  new NoSuchElementException
  }

  //Q3 ans
  def nthRecursive[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (0, h :: _   ) => h
    case (n, _ :: tail) => nthRecursive(n - 1, tail)
    case (_, Nil      ) => throw new NoSuchElementException
  }

  //Q4
  def length[A](ls : List[A]): Int = ls match {
    case Nil => 0
    case _::tail => 1+length(tail)
  }

  //Q4 ans
  def lengthTailRecursive[A](ls: List[A]): Int = {
    def lengthR(result: Int, curList: List[A]): Int = curList match {
      case Nil       => result
      case _ :: tail => lengthR(result + 1, tail)
    }
    lengthR(0, ls)
  }

  //Q5
  def reverse[A](ls : List[A]): List[A] = ls match{
    case Nil => Nil
    case h :: tail => reverse(tail) ::: List(h)
    case h :: Nil => List(h)
  }

  def reverse2[A](ls : List[A]): List[A] = {
    def rr(cur:List[A], left:List[A]): List[A] = left match{
      case Nil => cur
      case h :: tail => rr(h::cur,tail)
      case h :: Nil => h::cur // could be in case h::tail
    }
    rr(List(),ls)
  }

  //Q5 ans
  def reverseTailRecursive[A](ls: List[A]): List[A] = {
    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil       => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }

  //Q6
  def isPalindrome[A](ls :List[A]) :Boolean = {
    val r = ls.reverse
    r==ls
  }

  //Q6 ans
  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse


}
