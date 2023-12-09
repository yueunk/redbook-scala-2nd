package fpinscala.exercises.parallelism

import java.util.concurrent.*

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  private case class UnitFuture[A](get: A) extends Future[A]:
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get

  def unit[A](a: A): Par[A] = es => UnitFuture(a) // wrap the value with a future via executor service

  def fork[A](a: => Par[A]): Par[A] = // lazy version of unit
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a)) // lazy version of unit

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](pas: List[Par[A]]): Par[List[A]] = {
    // list of ParA -> Par of ListA
    val acc: Par[List[A]] = unit(List.empty[A])
    pas.foldRight(acc)((pa, acc) => pa.map2(acc)(_ :: _))
  }
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
    // 1. map over the given list of A, and return list of Par of B
    val fbs: List[Par[B]] = as.map(asyncF(f))

    // 2. convert list of ParB to par of ListB
    sequence(fbs)
  }

  def parFilter[A, B](as: List[A])(f: A => Boolean): Par[List[A]] = {
    // filter as, then convert to Par of as
    lazyUnit(as.filter(f)) // need the predicate on its own thread
  }
  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val futureA = pa(es)
      val futureB = pb(es)
      val c = f(futureA.get, futureB.get)
      UnitFuture(c)
    }

    def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => new Future[C] {
      private val futureA = pa(es)
      private val futureB = pb(es)

      override def cancel(mayInterruptIfRunning: Boolean): Boolean = futureA.cancel(mayInterruptIfRunning) || futureB.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled

      override def isDone: Boolean = futureA.isDone && futureB.isDone

      // "no timeout" in nanoseconds
      override def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

      override def get(timeout: Long, unit: TimeUnit): C = {
        /*
        - get start time
        - timeout is the limit
          - get the difference between now and start time, which is the elapsed time
          - timeout - elapsed will give you time left in the limit
          - the second future should use that limitLeft
        */
        val timeoutInNano = TimeUnit.NANOSECONDS.convert(timeout, unit)
        val startTime = System.nanoTime()
        val a = futureA.get(timeout, TimeUnit.NANOSECONDS)
        val timeElapsed = System.nanoTime() - startTime
        val timeLeft = timeoutInNano - timeElapsed
        val b = futureB.get(timeLeft, TimeUnit.NANOSECONDS)

        f(a, b)
      }
    }


