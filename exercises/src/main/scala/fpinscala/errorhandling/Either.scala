package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => Right(x)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(x => b.map(y => f(x, y)))
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object TestEither {

  def main(args: Array[String]): Unit = {
    assert(Right(4).map(2.*) == Right(8))
    assert((Left("foo"): Either[String, Int]).map(2.*) == Left("foo"))

    assert(Right(4).flatMap(x => Right("a" * x)) == Right("aaaa"))
    assert(Right(4).flatMap(x => Left("Error")) == Left("Error"))
    assert(Left("foo").flatMap(x => Right(x)) == Left("foo"))

    assert(Right(4).orElse(Right("bar")) == Right(4))
    assert(Left("foo").orElse(Right("bar")) == Right("bar"))
    assert(Left("foo").orElse(Left("bar")) == Left("bar"))

    assert(Right(2).map2(Right("s"))((a, b) => b * a) == Right("ss"))
    assert(Right(2).map2(Left("s"): Either[String, String])((a, b) => b * a) == Left("s"))
    assert((Left("bar") : Either[String, Int]).map2(Right("test"))((a, b) => b * a) == Left("bar"))
    assert((Left("foo") : Either[String, Int]).map2(Left("s"): Either[String, String])((a, b) => b * a) == Left("foo"))
  }
}

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => f(h).flatMap(x => traverse(t)(f).map(y => x :: y))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}

object TestTraverse {

  import Either.traverse

  def main(args: Array[String]): Unit = {
    assert(traverse(List(1, 2, 3, 4, 5, 6))(x => Right(x)) == Right(List(1, 2, 3, 4, 5, 6)))
    assert(traverse(List(1, 2, 3, 4, 5, 6))(x => if (x == 1) Left("error") else Right(x)) == Left("error"))
    assert(traverse(List(1, 2, 3, 4, 5, 6))(x => if (x == 2) Left("error") else Right(x)) == Left("error"))
    assert(traverse(List(1, 2, 3, 4, 5, 6))(x => if (x == 3) Left("error") else Right(x)) == Left("error"))
    assert(traverse(List(1, 2, 3, 4, 5, 6))(x => if (x == 4) Left("error") else Right(x)) == Left("error"))
    assert(traverse(List(1, 2, 3, 4, 5, 6))(x => if (x == 5) Left("error") else Right(x)) == Left("error"))
    assert(traverse(List(1, 2, 3, 4, 5, 6))(x => if (x == 6) Left("error") else Right(x)) == Left("error"))
    assert(traverse(List(1, 2, 3, 4, 5, 6))(x => if (x % 2 == 1) Left("error" + x.toString) else Right(x)) == Left("error1"))
  }
}

object TestSequence {

  import Either.sequence

  def main(args: Array[String]): Unit = {
    assert(sequence(List(Right(1), Right(2), Right(3), Right(4))) == Right(List(1, 2, 3, 4)))
    assert(sequence(List(Left("error"), Right(2), Right(3), Right(4))) == Left("error"))
    assert(sequence(List(Right(1), Left("error"), Right(3), Right(4))) == Left("error"))
    assert(sequence(List(Right(1), Right(2), Left("error"), Right(4))) == Left("error"))
    assert(sequence(List(Right(1), Right(2), Right(3), Left("error"))) == Left("error"))
    assert(sequence(List(Right(1), Left("error1"), Right(3), Left("error2"))) == Left("error1"))
  }
}