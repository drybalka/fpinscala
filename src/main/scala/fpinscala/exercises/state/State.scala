package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.


object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    if i != Int.MinValue then (math.abs(i), r) else (0, r)

  def double(rng: RNG): (Double, RNG) =
    map(RNG.nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)
  // def double(rng: RNG): (Double, RNG) =
  //   val (i, r) = RNG.nonNegativeInt(rng)
  //   (i.toDouble / Int.MaxValue, r)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = RNG.double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (i, r1) = rng.nextInt
    val (d, r2) = RNG.double(r1)
    ((d, i), r2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, r1) = RNG.double(rng)
    val (d2, r2) = RNG.double(r1)
    val (d3, r3) = RNG.double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (1 to count).foldLeft((Nil: List[Int], rng)): (tuple, _) =>
      val (l, r) = tuple
      val (i, r2) = r.nextInt
      (i :: l, r2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, ra2) = ra(rng)
      val (b, rb2) = rb(ra2)
      (f(a, b), rb2)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      rs.foldRight((Nil: List[A], rng)): (r, acc) =>
        val (l, racc) = acc
        val (a, ra) = r(racc)
        (a :: l, ra)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, r2) = r(rng)
      f(a)(r2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => rng => (f(a), rng))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] =
    flatMap(ra)(a =>
      rng =>
        rb(rng) match
          case (b, r) => (f(a, b), r)
    )


opaque type State[S, +A] = S => (A, S)


object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => s => (f(a), s))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a =>
        s =>
          sb(s) match
            case (b, s2) => (f(a, b), s2)
      )

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s2) = underlying.run(s)
        f(a)(s2)

  def apply[S, A](f: S => (A, S)): State[S, A] = f


enum Input:
  case Coin, Turn


case class Machine(locked: Boolean, candies: Int, coins: Int)


object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(machine =>
      val endState = inputs.foldLeft(machine)((m, input) =>
        input match
          case Input.Coin => simulateCoin(m)
          case Input.Turn => simulateTurn(m)
      )
      ((endState.coins, endState.candies), endState)
    )

  def simulateCoin(m: Machine): Machine =
    if m.candies <= 0 || !m.locked then m
    else m.copy(locked = false, coins = m.coins + 1)

  def simulateTurn(m: Machine): Machine =
    if m.candies <= 0 || m.locked then m
    else m.copy(locked = true, candies = m.candies - 1)
