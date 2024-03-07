package fpinscala.exercises.parsing


import fpinscala.answers.testing.*
import scala.util.matching.Regex
import javax.security.auth.login.FailedLoginException


trait Parsers[Parser[+_]]:
  // self => // so inner classes may call methods of trait

  def string(s: String): Parser[String]
  def regex(r: Regex): Parser[String]
  def succeed[A](a: A): Parser[A]
  // def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def fail(msg: String): Parser[Nothing]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def boolean: Parser[Boolean] =
    string("true").map(_ => true) | string("false").map(_ => false)
  def number: Parser[Double] =
    regex("""[0-9\.e]+""".r).map(_.toDouble).label("number")
  // def number: Parser[Double] = regex("""[0-9]+""".r).map(_.toDouble)
  // def scientific: Parser[Double] = regex("""[0-9]+e[0-9]+""".r).map(_.toDouble)
  def quotedString: Parser[String] = regex("""\"[^\"]*\"""".r)

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]
    def label(msg: String): Parser[A]
    def scope(msg: String): Parser[A]
    def attempt: Parser[A]

    def flatMap[B](f: A => Parser[B]): Parser[B]
    def slice: Parser[String]

    infix def product[B](p2: => Parser[B]): Parser[(A, B)] =
      p.flatMap(a => p2.map(b => (a, b)))
    def **[B](p2: => Parser[B]): Parser[(A, B)] = p.product(p2)

    infix def skipLeft[B](p2: Parser[B]): Parser[B] = p.product(p2).map(_._2)
    infix def skipRight[B](p2: Parser[B]): Parser[A] = p.product(p2).map(_._1)

    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def map[B](f: A => B): Parser[B] =
      p.flatMap(a => succeed(f(a)))
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] =
      (p ** p2).map(f.tupled)
      // p.flatMap(a => p2.map(b => f(a, b)))
    def many: Parser[List[A]] =
      (p ** p.many).map(_ :: _) | succeed(Nil)
      // p.flatMap(a => p.many.map(tail => a :: tail)) | succeed(Nil)
    def many1: Parser[List[A]] =
      (p ** p.many).map(_ :: _)
      // p.flatMap(a => p.many.map(tail => a :: tail))
    def manyWithSeparator(sep: Parser[Any]): Parser[List[A]] =
      (p ** (sep skipLeft p).many).map(_ :: _) | succeed(Nil)
    def listOfN(n: Int): Parser[List[A]] =
      if n == 0 then succeed(Nil)
      else (p ** p.listOfN(n - 1)).map(_ :: _)

    def option: Parser[Option[A]] = p.map(Some.apply) | succeed(None)

    def trim: Parser[A] =
      val whitespace = regex("""\s*""".r)
      whitespace skipLeft p skipRight whitespace

  case class ParserOps[A](p: Parser[A])

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
    def flatMapLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      equal(p2, p1.flatMap(_ => p2))(in)

    def sliceLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p.slice.run(s).map(p.run) == p.run(s))

    def charLaw(in: Gen[Char]): Prop =
      Prop.forAll(in)(c => char(c).run(c.toString) == Right(c))
    def stringLaw(in: Gen[String]): Prop =
      Prop.forAll(in)(s => string(s).run(s) == Right(s))
    def succeedLaw[A](in: Gen[(A, String)]): Prop =
      Prop.forAll(in)((a, s) => succeed(a).run(s) == Right(a))

    def orLaw[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s =>
        p1.run(s) match
          case Right(_) => (p1 | p2).run(s) == p1.run(s)
          case Left(_)  => (p1 | p2).run(s) == p2.run(s)
      )

    def productLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      Prop.forAll(in)(s =>
        (p1.run(s), p2.run(s)) match
          case (Left(l1), _)         => (p1 ** p2).run(s) == Left(l1)
          case (Right(r1), Left(l2)) => (p1 ** p2).run(s) == Left(l2)
          case (Right(r1), Right(r2)) =>
            (p1 ** p2).run(s) == (Right(r1), Right(r2))
      )


case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  def remaining: String = input.substring(offset)

  def slice(n: Int) = input.substring(offset, offset + n)

  def columnCaret = (" " * (col - 1)) + "^"

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if input.length > 1 then input.linesIterator.drop(line - 1).next()
    else ""


case class ParseError(
    stack: List[(Location, String)] = List(),
    otherFailures: List[ParseError] = List()
):
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(stack.lastOption.map(pair => (pair._1, s)).toList)

  override def toString =
    if stack.isEmpty then "no error message"
    else
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed
        .map((loc, msg) => s"${formatLoc(loc)} $msg")
        .mkString("\n") + context

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1)
      .view
      .mapValues(_.map(_._2).mkString("; "))
      .toList
      .sortBy(_._1.offset)

  def formatLoc(l: Location): String = s"${l.line}.${l.col}"


class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] =
    for
      nString <- regex("[0-9]+".r)
      n <- nString.toIntOption match
        case Some(n) => succeed(n)
        case None    => fail("expected an integer")
    yield n

  val nConsecutiveAs: Parser[Int] =
    for
      n <- nonNegativeInt
      _ <- char('a').listOfN(n)
    yield n


object MyParsers extends Parsers[MyParsers.Parser]:
  type Parser[+A] = Location => Result[A]

  import Result.{Success, Failure}
  enum Result[+A]:
    case Success(get: A, charsConsumed: Int)
    case Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

    def extract: Either[ParseError, A] = this match
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)

    def uncommit: Result[A] = this match
      case Failure(e, true) => Failure(e, false)
      case _                => this

    def addCommit(isCommitted: Boolean): Result[A] = this match
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this

    def mapError(f: ParseError => ParseError): Result[A] = this match
      case Failure(e, c) => Failure(f(e), c)
      case _             => this

    def advanceSuccess(n: Int): Result[A] = this match
      case Success(a, m) => Success(a, n + m)
      case _             => this

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int =
    var i = 0
    while i + offset < s1.length && i < s2.length do
      if s1.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s1.length - offset >= s2.length then -1
    else s1.length - offset

  def string(w: String): Parser[String] =
    l =>
      val i = firstNonmatchingIndex(l.input, w, l.offset)
      if i == -1 then // they matched
        Success(w, w.length)
      else Failure(l.advanceBy(i).toError(s"'$w'"), i != 0)

  def regex(r: Regex): Parser[String] =
    l =>
      r.findPrefixOf(l.remaining) match
        case None    => Failure(l.toError(s"regex $r"), false)
        case Some(m) => Success(m, m.length)

  def fail(msg: String): Parser[Nothing] =
    l => Failure(l.toError(msg), true)

  def succeed[A](a: A): Parser[A] =
    _ => Success(a, 0)

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A] =
      p(Location(input)).extract

    def or(p2: => Parser[A]): Parser[A] =
      l =>
        p(l) match
          case Failure(e, false) => p2(l)
          case r                 => r

    def attempt: Parser[A] = l => p(l).uncommit

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      l =>
        p(l) match
          case Success(a, n) =>
            f(a)(l.advanceBy(n))
              .addCommit(n != 0)
              .advanceSuccess(n)
          case f @ Failure(_, _) => f

    def slice: Parser[String] =
      l =>
        p(l) match
          case Success(_, n)     => Success(l.slice(n), n)
          case f @ Failure(_, _) => f

    def label(msg: String): Parser[A] =
      l => p(l).mapError(_.push(l, msg))

    def scope(msg: String): Parser[A] =
      l => p(l).mapError(_.push(l, msg))
