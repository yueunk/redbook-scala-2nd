package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}
import scala.math

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None => None

  def getOrElse[B>:A](default: => B): B = this match // B is supertype of A or A
    case Some(a) => a
    case None => default

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None) // map to nested Option, then getOrElse to remove the outer layer
//    this match
//    case Some(a) => f(a)
//    case None => None


// orElse only evaluates ob if needed
  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(a => Some(a)).getOrElse(ob)
  //    this match
//    case Some(a) => Some(a)
//    case None => ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)
//    this match
//    case Some(a) if f(a) => Some(a)
//    case _ => None

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    // if mean exists continue the computation for variance
    // - for each x, get mean; x becomes Option[Double]
    //   def flatMap[B](f: A => Option[B]): Option[B]
    // - for each mean, get variance; math.pow(x - m, 2)
    // A is x which is Double
    // Option[B] is Option[Double] and mean returns Option[Double]
//    val m: Option[Double] = mean(xs)
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    // if m is Some, then compute variance
    // else return None


  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    // list can be Nil or Cons(h, t) equivalent h :: t
    as.foldRight[Option[List[A]]](Some(Nil))((o, acc) => map2(o, acc)(_ :: _)) // why does it not compile without specifying the type parameter for foldRight?
//    case Nil => Some(Nil)
//    case List(Some(a)) => Some(as.map(_ => a))
//    case _ => None


    // input: a list of Options
    // output: an option of a List
    // if the list includes any None, return None
    // otherwise, convert List[Option[A]] to List[A] then wrap it with Option to return


  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    // given a list, traverse once to return an option of the list after transforming. if there's any None elem, return None
    // - fold with an accumulator matching the return type of Option of List of B
    // - each element a will be passed to the function and transformed to another Option
    as.foldRight(Some(Nil): Option[List[B]])((o, acc) => map2(f(o), acc)(_ :: _)) // only call map2 if Some
