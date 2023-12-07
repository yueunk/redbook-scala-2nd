package fpinscala.exercises.parallelism

import java.util.concurrent.*

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  private case class UnitFuture[A](get: A) extends Future[A]:
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))