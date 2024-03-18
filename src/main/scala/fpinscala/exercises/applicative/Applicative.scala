package fpinscala.exercises.applicative


import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State


trait Applicative[F[_]] extends Functor[F]:
  self =>

  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    fab.map2(fa)((f, a) => f(a))

  extension [A](fa: F[A])
    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      apply(apply(unit(f.curried))(fa))(fb)

    def map[B](f: A => B): F[B] =
      apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = fas match
    case Nil          => unit(Nil)
    case head :: next => head.map2(sequence(next))(_ :: _)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    sequence(as.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    fa.map2(unit(()))((a, _) => List.fill(n)(a))

  extension [A](fa: F[A])
    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

    def map3[B, C, D](
        fb: F[B],
        fc: F[C]
    )(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map4[B, C, D, E](
        fb: F[B],
        fc: F[C],
        fd: F[D]
    )(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] = new:
    def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
    extension [A](fga: (F[A], G[A]))
      override def map2[B, C](fgb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fga._1)(fgb._1)(f), G.map2(fga._2)(fgb._2)(f))

  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] = new:
    def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
    extension [A](fa: F[G[A]])
      override def map2[B, C](fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa)(fb)((ga: G[A], gb: G[B]) => G.map2(ga)(gb)(f))

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map()))((pair, acc) =>
      val (k, fv) = pair
      acc.map2(fv)((am, v) => am + (k -> v))
    )


object Applicative:
  opaque type ZipList[+A] = LazyList[A]

  object ZipList:
    def fromLazyList[A](la: LazyList[A]): ZipList[A] = la
    extension [A](za: ZipList[A]) def toLazyList: LazyList[A] = za

    given zipListApplicative: Applicative[ZipList] with
      def unit[A](a: => A): ZipList[A] =
        LazyList.continually(a)
      extension [A](fa: ZipList[A])
        override def map2[B, C](fb: ZipList[B])(f: (A, B) => C) =
          fa.zip(fb).map(f.tupled)

  enum Validated[+E, +A]:
    case Valid(get: A) extends Validated[Nothing, A]
    case Invalid(error: E) extends Validated[E, Nothing]

  object Validated:
    given validatedApplicative[E: Monoid]: Applicative[[x] =>> Validated[E, x]]
    with
      def unit[A](a: => A) = Valid(a)
      extension [A](fa: Validated[E, A])
        override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C) =
          (fa, fb) match
            case (Valid(a), Valid(b))   => Valid(f(a, b))
            case (Valid(a), Invalid(b)) => Invalid(b)
            case (Invalid(a), Valid(b)) => Invalid(a)
            case (Invalid(a), Invalid(b)) =>
              Invalid(summon[Monoid[E]].combine(a, b))

  type Const[A, B] = A

  given monoidApplicative[M](using
      m: Monoid[M]
  ): Applicative[[x] =>> Const[M, x]] with
    def unit[A](a: => A): M = m.empty
    override def apply[A, B](m1: M)(m2: M): M = m.combine(m1, m2)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A): Option[A] = Some(a)
    extension [A](oa: Option[A])
      override def flatMap[B](f: A => Option[B]) = oa.flatMap(f)

  given eitherMonad[E]: Monad[[x] =>> Either[E, x]] with
    def unit[A](a: => A): Either[E, A] = ???
    extension [A](eea: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]) = ???

  given stateMonad[S]: Monad[[x] =>> State[S, x]] with
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    extension [A](st: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(st)(f)
