package fpinscala.exercises.laziness

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => ::(h(), t().toList) // t is a list. t() is a LazyList[A]. calling toList on t() should be right?

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => LazyList.cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => LazyList.cons(h(), LazyList.empty)
    case _ => Empty
    // three scenarios. List is empty. List is non-empty, and n is 0. n is greater than 0


  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n == 0 => this
    case Cons(h, t) if n == 1 => t()
    case Cons(h, t) if n > 1 => t().drop(n - 1) // dropping the head
    case _ => Empty

//  def takeWhile(p: A => Boolean): LazyList[A] = this match
//    case Cons(h, t) if p(h()) => LazyList.cons(h(), t().takeWhile(p))
//    case _ => Empty

  def takeWhile(p: A => Boolean): LazyList[A] = // using foldRight
    // same as filter. we want accum to be an empty LazyList
    // for each head, evaluate if it's true when passed to p
    // if true, then return a LazyList including a as a head, and the accum list as a tail
    // otherwise, return an empty list
    foldRight(LazyList.empty)((a, ll) => if p(a) then LazyList.cons(a, ll) else LazyList.empty)

  def forAll(p: A => Boolean): Boolean = this match
    case Cons(h, t) => p(h()) || t().forAll(p)
    case Empty => true
  // early termination when p returns false
  // iterate through elements of the list
  // - if the current head is passed to p and returns false, return false
  // - else keep evaluating

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] = foldRight(LazyList.empty: LazyList[B])((a, ll) => LazyList.cons(f(a), ll))
  def filter(f: A => Boolean): LazyList[A] = foldRight(LazyList.empty: LazyList[A])((a, ll) => if f(a) then LazyList.cons(a, ll) else LazyList.empty)
  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = foldRight(that)((l, ll) => LazyList.cons(l, ll))
  // append a LazyList non strictly
  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight(LazyList.empty: LazyList[B])((a, ll) => f(a).append(ll))

  def startsWith[B](that: LazyList[B]): Boolean = (this, that) match
    case (Cons(h, t), Cons(h2, t2)) if h() == h2() => t().startsWith(t2())
    case (_, Empty) => true
    case _ => false

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(this):
      case Cons(h, t) => Some(f(h()), t())
      case _ => None

  def takeViaUnfold(n: Int): LazyList[A] =
    LazyList.unfold((this, n)): // remember the state can contain various states! not a single value
        // if the list is empty return NOne
        // if the n is 0 return None
        // if the n is 1, return head
        // if the n is 2, return head and next's head
      case (Cons(h, t), n) if n == 0 => None
      case (Cons(h, t), n) if n == 1 => Some(h(), (LazyList.empty, 0)) // current value, and state
      case (Cons(h, t), n) => Some(h(), (t(), n - 1))
      case _ => None

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    LazyList.unfold(this):
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case _ => None

  def tails: LazyList[LazyList[A]] =
    LazyList.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((Cons(h, t), t()))
    }.append(LazyList(LazyList.empty))

  def scanRight[B](i: B)(f: (A, => B) => B): LazyList[B] =
    foldRight((i, LazyList(i))) { (a, acc) =>
      val b2 = f(a, acc._1)
      (b2, LazyList.cons(b2, acc._2))
    }._2





object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = {
    def go(curr: Int, next: Int): LazyList[Int] = {
      LazyList.cons(curr, go(next, curr + next))
    }
    go(0, 1)
  }

  // recursive vs. corecursive
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case Some((a, s)) => LazyList.cons(a, unfold(s)(f))
    case None => LazyList.empty

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))((curr, next) => Some((curr, (next, curr + next))))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(curr => Some(curr, curr + 1))
  // from returns a lazy list of integers from the given n
  // each state will be updated to an integer incremented by 1

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(curr => Some(curr, curr))

  lazy val onesViaUnfold: LazyList[Int] = unfold(1)(_ => Some(1, 1))


