package fpinscala.exercises.laziness


enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if f(h()) then Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    if n <= 0 then Empty
    else
      this match
        case Cons(h, t) => LazyList.cons(h(), t().take(n - 1))
        case Empty      => Empty

  def drop(n: Int): LazyList[A] =
    if n <= 0 then this
    else
      this match
        case Empty      => Empty
        case Cons(h, t) => t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Empty => Empty
    case Cons(h, t) =>
      val hh = h()
      if p(hh) then Cons(() => hh, () => t().takeWhile(p))
      else Empty

  // def forAll(p: A => Boolean): Boolean = !this.exists(!p(_))
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    this.foldRight(Empty: LazyList[B])((a, acc) => Cons(() => f(a), () => acc))

  def filter(p: A => Boolean): LazyList[A] =
    this.foldRight(Empty: LazyList[A])((a, acc) =>
      if p(a) then Cons(() => a, () => acc) else acc
    )

  // def append[B >: A](b: B): LazyList[B] = Cons(() => b, () => this)
  def append[B >: A](other: LazyList[B]): LazyList[B] =
    this.foldRight(other)((a, acc) => Cons(() => a, () => acc))

  def flatmap[B](f: A => LazyList[B]): LazyList[B] =
    this.foldRight(Empty: LazyList[B])((a, acc) => f(a).append(acc))

  def startsWith[B](s: LazyList[B]): Boolean = s match
    case Empty => true
    case Cons(sh, st) =>
      this match
        case Cons(h, t) if h() == sh() => t().startsWith(st())
        case _                         => false


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

  lazy val fibs: LazyList[Int] =
    def from2(a: Int, b: Int): LazyList[Int] = LazyList.cons(a, from2(b, a + b))
    from2(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None       => Empty
      case Some(a, s) => LazyList.cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))((a, b) => Some(a, (b, a + b)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(n => Some(n, n + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(_ => Some(a, a))

  lazy val onesViaUnfold: LazyList[Int] = unfold(1)(_ => Some(1, 1))
