package nico

import NStack.{EmptyStack, NItem}

import scala.language.postfixOps


sealed trait NStack[+A] {

  def push[U >: A](value: U): NStack[U]

  def pull(): (Option[A], NStack[A])

  def size(): Int

  def toList(): List[A] = this match {
    case EmptyStack           =>  List.empty
    case NItem(v, None)       =>  List(v)
    case NItem(v, Some(tail)) =>  v :: tail.toList()
  }

  def head(): Option[A] = this match {
    case EmptyStack   =>  None
    case NItem(v, _)  =>  Some(v)
  }
}

object NStack {
  def empty[A]: NStack[A] = EmptyStack.asInstanceOf[NStack[A]]

  def apply[A](value: A): NStack[A] = NItem(value)

  case object EmptyStack extends NStack[Nothing] {
    override def size(): Int = 0

    override def push[U >: Nothing](value: U): NStack[U] = NStack(value)

    override def pull(): (Option[Nothing], NStack[Nothing]) = (None, this)
  }

  case class NItem[+A](a: A, tail: Option[NItem[A]] = None) extends NStack[A] {

    override def size(): Int = tail match {
      case None       =>  1
      case Some(item) =>  1 + item.size()
    }

    override def push[U >: A](value: U): NStack[U] = NItem(value, Some(this))

    override def pull(): (Option[A], NStack[A]) = (Some(a), tail.get)
  }
}


class FactorialFunc(n: BigInt) extends ( () => BigInt) {

  private lazy val result = (1 to n.toInt).foldLeft(BigInt(1))(_ * _)
  
  override def apply(): BigInt = result

  override def toString(): String = (1 to n.toInt).map(_.toString).mkString(""," * ", " = " + result)
}


object UsingExt {
  import Ext._

  def factorial(n: Int): BigInt = {
    val fact = n!

    println(fact)

    fact()
  }

}

object Ext {
  implicit class Ops(n: Int) {
    def !() = new FactorialFunc(BigInt(n))
  }
}


class Sum(a: Int, b: Int) extends (() => Int) {
  override def apply(): Int = a + b

  override def toString(): String = s"SUM($a, $b)"
}

object Sum {
  def apply(a: Int, b: Int): Sum = new Sum(a, b)
}

class Power extends ((Int, Int) => Int) {
  override def apply(v1: Int, v2: Int): Int = (1 to v2).foldLeft(v1){ case (acc, x) => acc * x }
}

object UsingSum {


  def xx = Sum(5,5)()

  def pw(a: Int, b: Int) = new Power().apply(a, b)


}