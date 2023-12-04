package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match // exercise 3.1
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 3
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("tail is Nil")
    case Cons(_, t) => t

  // exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("tail is Nil")
    case Cons(_, t) => Cons(h, t)

  // exercise 3.4
  // - drop n elements
  // - each call will pass in n - 1 til n reaches 0, where only tail is returned each time
  def drop[A](l: List[A], n: Int): List[A] =
    if (l == Nil) Nil
    else if (n > 0) drop(tail(l), n - 1)
    else l

  // exercise 3.5
  // - as long as f(elem) returns true, drop the prefix
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l


  // exercise 3.6
  // - remove the last elem from the List
  // - if the tail length is 2, replace that with just the head
  // -- risk of stack overflow since there's no data sharing, rather copying of data
  def init[A](l: List[A]): List[A] = l match
    case Nil => sys.error("list is empty")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))

  def length[A](l: List[A]): Int = l match
    case Nil => 0
    case Cons(_, t) => length(t) + 1

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    // put the current element as a new head
    foldLeft(l, List[A](), (newL, elem) => Cons(elem, newL))
    // [], 1 -> [1]
    // [1], 2 -> [2, 1]
    // [2, 1], 3 -> [3, 2, 1]

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    // [1,2,3], [4,5,6] => [1,2,3,4,5,6]
    foldRight(l, r, (r, elem) => Cons(r, elem))

  def concat[A](l: List[List[A]]): List[A] =
    // list of list [[1, 2, 3], [4, 5, 6]] => [1, 2, 3, 4, 5, 6] in O(N)
    def flatten(listElem: List[A], acc: List[A]) = foldRight(listElem, acc, (elem, acc) => Cons(elem, acc))
    foldLeft(l, List[A](), (acc, elem) => flatten(acc, elem))

  def incrementEach(l: List[Int]): List[Int] =
    // given a list, return a list with each elem incremented by 1
    // [1, 2, 3] => [2, 3, 4]
    // [], 1 => [2]
    // [2], 2 => [2, 3]
    foldRight(l, List[Int](), (elem, acc) => Cons(elem + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String](), (elem, acc) => Cons(elem.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRight(l, List[B](), (elem, acc) => Cons(f(elem), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, List[A](), (elem, acc) => {
      if (f(elem)) Cons(elem, acc) else acc
    })

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
  // [1, 2, 3], [["one"], ["two"], ["three"]] => ["one", "two", "three"]
  // [], 1 => ["one"], ["one"]
  // ["one"], 2 => ["two"], ["one", "two"]
  // ["one", "two"], 3 => ["three"], ["one", "two", "three"]
  // for each listElem, fold right to put each elem into the accum
    val acc = List[B]()
    foldRight(as, acc, (elem, acc) => {
      foldRight(f(elem), acc, (innerElem, innerAcc) => Cons(innerElem, innerAcc))
    })

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil) // redo

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    // return a list of Int that represent the sum of the two elems from the same index
    //

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = (a, b) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
    // factoring out the + since the types from a and b can be different. instead we pass in a function.

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => hasSubsequence(t1, t2)
    case (Cons(_, t1), sub) => hasSubsequence(t1, sub)
    // if sup includes sub then return true
    // [1, 2, 3], [1, 2]
    // head match; continue evaluating (next head and next head)

    // [1, 2, 3] [2, 3]
    // head does not match; continue evaluating (next head and curr head)

