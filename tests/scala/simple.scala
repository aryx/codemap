package test

import scala.collection.mutable

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color

object Simple {
  def factorial(n: Int): Int = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val msg = "hello"
    val x = 42
    val b = true
    println(factorial(x))
  }
}
