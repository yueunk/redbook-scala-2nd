package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG) // it's a type, given rng, return a and rng

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => // given rng, return (a, rng)
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)
  /*
  - nextInt returns a random number between min and maxValue
  - here, we are interested in getting a NON-negative integer
  - say we have a minValue, which is negative, and times -1, that does not exist. -10 -> +10 +10 does not exist? -9
  */

  def double: Rand[Double] =
// reimplement using map, which takes state and transformation, where int to double transformation can happen
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 0.1))

  //    val (i, r) = nonNegativeInt(rng)
//    (i / (Int.MaxValue.toDouble + 0.1), r)
    // the max i value will be Int.MaxValue, and if you divide that by Int.MaxValue that would be 1. the requirement is that 1 non inclusive, so in order to accomodate that divide the maxValue by a slightly bigger number by adding 1

    /*
    nextInt returns a random Int between min and maxVal, minVal doesn't have non-negative couterpart, so we handled that by incrementing it by 1 then multiply by -1
    now we want to return a random Double that is between 0 and 1
    so that means we care about values from 0 and maxValue
    */

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, r) = nonNegativeInt(rng) // Int, RNG
    val (di, _) = double(rng) // Double
    ((i, di), r)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val (i, r) = nonNegativeInt(rng) // Int, RNG
    val (di, _) = double(rng) // Double
    ((di, i), r)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (di1, r1) = double(rng) // Double
    val (di2, r2) = double(r1) // Double
    val (di3, r3) = double(r2) // Double
    ((di1, di2, di3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = // redo - List[Int] could be Nil, as in empty!
    // given a count and rng, return a list of random integers and the next rng state
    // - when count reaches 0, return the list and next rng
    // - otherwise, get the next int and next rng to pass to the next iteration with count decremented by 1
    // - starting point will be a Nil list, given count and rng
    def go(count: Int, rng: RNG, list: List[Int]): (List[Int], RNG) =
      if (count <= 0) (list, rng)
      else
        val (i, rng2) = rng.nextInt
        go(count - 1, rng2, i :: list)

    go(count, rng, Nil)

  /*
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)
  */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)

    // given two actions, return a new action; each action has a tuple of value and state



  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    // note that unit is a function defined in Rand class, where it takes A and returns Rand of A
    // here, our A is List[A]
    rs.foldRight(unit(Nil: List[A]))((a, acc) => map2(a, acc)((a, acc) => a :: acc))


  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    // given Rand[A], transform the elem a to Rand[B] then return
    rng =>
      val (a, nextRng) = r(rng)
      val rb = f(a)
      rb(nextRng) // f(a) is Rand[B], and Rand is a type that takes a RNG and returns a tuple


  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))


  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      // given a function, return the state w/ the type B
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for { // flatMap is defined as given a, return State w/ b
        a <- underlying // on the left side, we are extracting the value of the type
        b <- sb // same, we are extracting the value of the type
      } yield f(a, b)


    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = state => (a, state)// return the type

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit[S, List[A]](Nil))((s, acc) => s.map2(acc)((a, listA) => a :: listA))

  def get = ???

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  // (Int, Int) refers to # coins and # candies left in the machine
  // no candy? do nothing.
  // 1. machine state: locked
  // - insert coin -> coin + 1 and state changes to unlocked
  // 2. machine state: unlocked
  // - turn knob -> candy - 1 and state changes to locked

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ??? // inputs.foreach(input => update(input, State.get))

  def update(input: Input, machine: Machine): Machine = (input, machine) match {
    case (_, Machine(_, 0, _)) => machine
    case (Input.Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
    case (Input.Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    case _ => machine
  }
  // given a list of inputs (coin or turn), return a new state
  // how do you access Machine from Candy object?
  // for the given input, we will perform the actions and the result depends on each state

// here we need to use State wrapper
// unit (a => State)
// map (a => b, return State b)
// map2 (given State b and a, b => C, return State c)
// flatMap (given a => State b, return State b)
// apply returns a function that depends on State
// underlying
// run returns State that depends state
// sequence returns a State of list of vals from list of States