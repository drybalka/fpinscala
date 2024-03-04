package fpinscala.exercises.testing


import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors, ExecutorService}


/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

trait Prop


object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???


object Gen:
  def unit[A](a: => A): Gen[A] = State((a, _))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map((i: Int) =>
      start + (i % (stopExclusive - start))
    )

  def boolean: Gen[Boolean] =
    State(RNG.nonNegativeInt).map(_ % 2 == 0)

  def double: Gen[Double] = State:
    RNG.double

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(if _ then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val norm = g1._2 / (g1._2 + g2._2)
    Gen.double.flatMap(d => if d < norm then g1._1 else g2._1)

  extension [A](self: Gen[A])
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def listOfN(n: Int): Gen[List[A]] =
      if n <= 0 then State((Nil, _))
      else
        State: rng =>
          val (tail, nextRNG) = self.listOfN(n - 1).run(rng)
          self.map(a => a :: tail).run(nextRNG)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(self.listOfN)

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      fpinscala.exercises.state.State.flatMap(self)(f)

    def unsized: SGen[A] = _ => self

    def list: SGen[List[A]] = n => listOfN(n)

    def nonEmptyList: SGen[List[A]] = n => listOfN(n + 1)


opaque type Gen[+A] = State[RNG, A]

// trait Gen[A]:
//   def map[B](f: A => B): Gen[B] = ???
//   def flatMap[B](f: A => Gen[B]): Gen[B] = ???

// trait SGen[+A]


object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  extension [A](self: SGen[A])
    def apply(n: Int): Gen[A] = self(n)
    def map[B](f: A => B): SGen[B] = n => self(n).map(f)
    def flatMap[B](f: A => SGen[B]): SGen[B] =
      n => self(n).flatMap(a => f(a)(n))


opaque type SGen[+A] = Int => Gen[A]
