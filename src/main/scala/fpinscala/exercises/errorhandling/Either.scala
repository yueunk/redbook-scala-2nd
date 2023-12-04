package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  // given a function that transforms type A to type B, return Either
  // - if f(a) returns an error, return Left with E
  // - otherwise return f(a)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e) => Left(e)
    case Right(a) => f(a)

  // only evaluates b if needed
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(_) => b
    case Right(a) => flatMap(a => Right(a))

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    a <- this // this being this Either.
    b <- b
  } yield f(a, b)

object Either:
  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  // input: a list of as, a function that returns an Either for each a transformed to b
  // output: an Either of list of b
  // foldRight to an accumulator that is Either
  // the starting point for the Either will b Right(Nil)
  // for each element a, we will transform it to Either[E, B]
  // for Each Either[E, B], transform into a list of B by prepending to the list
  // I don't know where E is defined...
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E,A](as: List[Either[E,A]]): Either[E,List[A]] =
    traverse(as)(a => a)
  // given a list of Either, return an Either of list

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = ???

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = ???

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = ???
