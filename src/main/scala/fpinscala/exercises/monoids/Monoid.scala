package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*


trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A


object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 + a2
    def empty = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    def empty: Int = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def empty: Boolean = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def empty: Boolean = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def empty: Option[A] = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(a1: A => A, a2: A => A): A => A = a1 andThen a2
    def empty: A => A = identity

  import fpinscala.exercises.testing.{Prop, Gen}
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(gen)(a => m.combine(a, m.empty) == a)

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldRight(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.empty)(m.combine)

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid)(a => b => f(a, b))(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid))(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.size == 0 then m.empty
    else if as.size == 1 then f(as(0))
    else
      val (l, r) = as.splitAt(as.size / 2)
      val lb = foldMapV(l, m)(f)
      val rb = foldMapV(r, m)(f)
      m.combine(lb, rb)

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def combine(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.combine)
    def empty: Par[A] = Par.unit(m.empty)

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap(bs => foldMapV(bs, par(m))(Par.lazyUnit))

  def ordered(ints: IndexedSeq[Int]): Boolean =
    val orderedMonoid = new Monoid[(Int, Int, Boolean)]:
      def combine(
          a1: (Int, Int, Boolean),
          a2: (Int, Int, Boolean)
      ): (Int, Int, Boolean) =
        (a1._1 min a2._1, a1._2 max a2._2, a1._3 && a2._3 && a1._2 < a2._1)
      def empty = (Int.MaxValue, Int.MinValue, true)
    foldMapV(ints, orderedMonoid)(i => (i, i, true))._3

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new:
    import WC.*
    def combine(a1: WC, a2: WC): WC = (a1, a2) match
      case (Stub(a), Stub(b))       => Stub(a + b)
      case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
      case (Part(l, w, r), Stub(b)) => Part(l, w, r + b)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + w2 + (if (r1 + l2).isEmpty then 0 else 1), r2)
    def empty: WC = Stub("")

  def count(s: String): Int =
    import WC.*
    def stub(c: Char): WC = c match
      case ' ' => Part("", 0, "")
      case c   => Stub(c.toString)
    def unstub(a: String): Int = if a.isEmpty then 0 else 1
    foldMapV(s.toIndexedSeq, wcMonoid)(stub) match
      case Stub(a)       => unstub(a)
      case Part(l, w, r) => unstub(l) + w + unstub(r)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)]
  with
    def combine(x: (A, B), y: (A, B)) =
      (ma.combine(x._1, y._1), mb.combine(x._2, y._2))
    val empty = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(empty): (acc, k) =>
        acc.updated(
          k,
          mv.combine(a.getOrElse(k, mv.empty), b.getOrElse(k, mv.empty))
        )
    val empty = Map()

  given Monoid[Int] with
    def combine(x: Int, y: Int) = x + y
    val empty = 0

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    import Foldable.given
    as.foldMap(a => Map(a -> 1))

end Monoid
