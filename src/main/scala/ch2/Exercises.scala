package ch2

object Exercises {
  def factorial(n: Int): Int = {
    @annotation.tailrec // compiler error if recursive call not in tail position
    def go(n: Int, acc: Int): Int = {
      if (n > 1) go(n - 1, acc * n)
      else acc
    }

    go(n, 1)
  }

  /*
   Exercise 2.1: write a tailrec fibonacci function
   - input: Int, output: Int
   - nth fib number is the sum of the previous two fib numbers (starting with 0, 1)
   - ex) 0, 1, 1, 2, 3, 5, 8, 13, 21, ...
   if n < 2 return n else fib(n - 1) + fib(n - 2)
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, curr: Int, next: Int): Int = {
      if (n <= 0) curr
      else go(n - 1, next, curr + next)
    }

    go(n, 0, 1)
  }

  println(fib(0)) // 0
  println(fib(1)) // 1
  println(fib(5)) // 5

  /*
  Listing 2.3: findFirst monomorphically
  - find the first index of the given key, or -1
  - input: arr of String, String
  - output: Int
   */
  def findFirst(arr: Array[String], key: String): Int = {
    @annotation.tailrec
    def go(idx: Int): Int = {
      if (idx == arr.length) -1
      else if (arr(idx) == key) idx
      else go(idx + 1)
    }

    go(0)
  }

  val arr: Array[String] = Array("a", "b", "c")
  println(findFirst(arr, "a"))
  println(findFirst(arr, "b"))
  println(findFirst(arr, "e"))

  def findFirstGeneric[A](arr: Array[A], f: A => Boolean): Int = {
    @annotation.tailrec
    def go(idx: Int): Int = {
      if (arr.length == idx) -1
      else if (f(arr(idx))) idx
      else go(idx + 1)
    }

    go(0)
  }

  val num: Array[Int] = Array(1, 2, 3)
  println(findFirstGeneric(num, (x: Int) => x == 1)) // note anonymous function (as opposed to named)
  println(findFirstGeneric(num, (x: Int) => x == 2))
  println(findFirstGeneric(num, (x: Int) => x == 5))

  /*
  Exercise 2.2: write a function isSorted that checks whether the given array (of a generic type) is sorted
  - input: Array[A], (A, A) => Boolean
  - output: Boolean
  - if array length < 2, return true
  - if first two items of the array are is sorted, evaluate the next two items
  - else return false
   */
  def isSorted[A](a: Array[A], p: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i == a.length - 1) true
      else if (p(a(i), a(i + 1))) loop(i + 1)
      else false
    }

    loop(0)
  }

  println(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x <= y)) // true
  println(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x > y)) // false

  /*
  Section 2.5: write a partial function of type A, B, C, that returns a function (B => C)
  - input: A, (A, B) => C
  - output: B => C
   */

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  /*
  Exercise 2.3: write an implementation of the curry function given the signature
  - note currying is given a function (A, B) => C, partially apply the function to return A => (B => C)
   */

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  /*
  Exercise 2.4: implement uncurry (reverse curry)
   */

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a, b) => f(a)(b)

  /*
  Exercise 2.5: implement compose (feed output of one to input of another func)
  - input: B => C, A => B
  - output: A => C
   */

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
