package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(x => Some(x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object OptionTest {

  def main(args: Array[String]): Unit = {
    assert(Some(2).map(4.*) == Some(8))
    assert((None: Option[Int]).map(4.*) == None)

    assert(Some("hello").getOrElse("foo") == "hello")
    assert((None: Option[String]).getOrElse("foo") == "foo")

    assert(Some(5).flatMap(x => Some(x.toString)) == Some("5"))
    assert((None: Option[Int]).flatMap(x => Some(x.toString)) == None)

    assert(Some(5).orElse(Some(10)) == Some(5))
    assert((None: Option[Int]).orElse(Some(10)) == Some(10))

    assert(Some(6).filter(x => x % 2 == 0) == Some(6))
    assert(Some(5).filter(x => x % 2 == 0) == None)
    assert((None: Option[Int]).filter(x => x % 2 == 0) == None)
  }
}


object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}

object TestVariance {

  import Option.variance

  def main(args: Array[String]): Unit = {
    assert(variance(List(1.0, 2.0, 3.0)).filter(x => math.abs(x - 1.0) < 1e9) != None)
    assert(variance(List()) == None)
  }
}