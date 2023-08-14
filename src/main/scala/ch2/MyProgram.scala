package ch2

import ch2.Exercises.factorial

import scala.math.abs

object MyProgram extends App {
  // HOF since it accepts a function as an argument
  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s value of %d is %d"
    msg.format(name, n, f(n))
  }

  def printAbsAndFactorial(): Unit = {
    println(formatResult("absolute", -42, abs))
    println(formatResult("factorial", 4, factorial))
  }

  printAbsAndFactorial()
}
