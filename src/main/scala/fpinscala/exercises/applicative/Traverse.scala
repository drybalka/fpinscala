package fpinscala.exercises.applicative


import fpinscala.answers.monads.Functor
import fpinscala.answers.state.State
import fpinscala.answers.monoids.{Monoid, Foldable}
import Applicative.Const


trait Traverse[F[_]] extends Functor[F], Foldable[F]:
  self =>

  extension [A](fa: F[A])
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
      fa.map(f).sequence

  extension [G[_]: Applicative, A](fga: F[G[A]])
    def sequence: G[F[A]] =
      fga.traverse(ga => ga)

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] =
      val om = summon[Monad[Option]]
      fa.traverse(a => Some(f(a)))(using om).get

    override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      // fa.traverse[Const[B, ?], Nothing](f)
      ???

    override def foldLeft[B](acc: B)(f: (B, A) => B): B =
      fa.mapAccum(acc)((a, b) => ((), f(b, a)))._2

    override def toList: List[A] =
      fa.mapAccum(List[A]())((a, s) => ((), a :: s))._2.reverse

    def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
      fa.traverse(a =>
        for
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        yield b
      ).run(s)

    def zipWithIndex: F[(A, Int)] =
      fa.mapAccum(0)((a, s) => ((a, s), s + 1))(0)

    def reverse: F[A] =
      fa.mapAccum(fa.toList.reverse)((_, s) => (s.head, s.tail))._1

    def fuse[M[_], N[_], B](f: A => M[B], g: A => N[B])(using
        m: Applicative[M],
        n: Applicative[N]
    ): (M[F[B]], N[F[B]]) =
      fa.traverse[[x] =>> (M[x], N[x]), B](a => (f(a), g(a)))(using
        m.product(n)
      )

  def compose[G[_]: Traverse]: Traverse[[x] =>> F[G[x]]] = new:
    extension [A](fa: F[G[A]])
      override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
        self.traverse(fa)(ga => ga.traverse(f))


case class Tree[+A](head: A, tail: List[Tree[A]])


object Traverse:
  given listTraverse: Traverse[List] with
    extension [A](as: List[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
        val g = summon[Applicative[G]]
        as.foldRight(g.unit(Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  given optionTraverse: Traverse[Option] with
    extension [A](oa: Option[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Option[B]] =
        val g = summon[Applicative[G]]
        oa match
          case None    => g.unit(None)
          case Some(a) => f(a).map(Some(_))

  given treeTraverse: Traverse[Tree] = new:
    extension [A](ta: Tree[A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
        val g = summon[Applicative[G]]
        f(ta.head).map2(ta.tail.traverse(a => a.traverse(f)))(Tree(_, _))

  given mapTraverse[K]: Traverse[[x] =>> Map[K, x]] with
    extension [A](m: Map[K, A])
      override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Map[K, B]] =
        val g = summon[Applicative[G]]
        m.foldLeft(g.unit(Map())):
          case (acc, (k, a)) =>
            acc.map2(f(a))((m, b) => m + (k -> b))
