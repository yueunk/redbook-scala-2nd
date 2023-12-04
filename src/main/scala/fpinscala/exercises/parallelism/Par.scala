package fpinscala.exercises.parallelism

import java.util.concurrent.*

trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}

trait Par[A] // this is a type.
object Par {
  opaque type Par[A] = ExecutorService => Future[A]

  // 1. unit to wrap a value into Par
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // 2. write a lazy version of unit
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 2. fork to lazy evaluate the input
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })
  extension[A] (pa: Par[A]) {
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val futureA = pa(es)
      val futureB = pb(es)
      UnitFuture(f(futureA.get, futureB.get))
    }

    def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = {
      es => new Future[C] {
        private val futureA = pa(es)
        private val futureB = pb(es)
        @volatile private var cache: Option[C] = None

        def isDone = cache.isDefined
        def get() => get(Long.MaxValue, TimeUnit.NANOSECONDS)
        def get(timeout: Long, units: TimeUnit) = {
          val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, units)
          val started = System.nanoTime
          val a = futureA.get(timeoutNs, TimeUnit.NANOSECONDS)
          val elapsed = System.nanoTime - started
          val b = futureB.get(timeoutMs - elapsed, TimeUnit.NANOSECONDS)
          val c = f(a, b)
          cache = Some(c)
          c
        }

        def isCancelled = futureA.isCancelled || futureB.isCancelled
        def cancel(evenIfRunning: Boolean) = futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)
      }
    }

    // run will spawn parallel computation and return the resulting value.
    def run(s: ExecutorService): Future[A] = pa(s)
  }
}
