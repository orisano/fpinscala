package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, acc) => p(x) && acc)

  def headOption: Option[A] = foldRight[Option[A]](None)((x, _) => Some(x))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: (A => B)): Stream[B] = foldRight[Stream[B]](Empty)((x, acc) => cons(f(x), acc))

  def filter(f: (A => Boolean)): Stream[A] = foldRight[Stream[A]](Empty)((x, acc) => if (f(x)) cons(x, acc) else acc)

  def append[AA >: A](s: => Stream[AA]): Stream[AA] = foldRight(s)((x, acc) => cons(x, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((x, acc) => f(x).append(acc))

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object TestTake {

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, cons(3, Empty))).take(2).toList == List(1, 2))
    assert(cons(1, cons(2, cons(3, Empty))).take(0).toList == Nil)
    assert(cons(1, cons(2, cons(3, Empty))).take(4).toList == List(1, 2, 3))
  }
}

object TestDrop {

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, cons(3, Empty))).drop(2).toList == List(3))
    assert(cons(1, cons(2, cons(3, Empty))).drop(0).toList == List(1, 2, 3))
    assert(cons(1, cons(2, cons(3, Empty))).drop(4).toList == Nil)
  }
}

object TestTakeWhile {

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, cons(3, Empty))).takeWhile(x => x < 3).toList == List(1, 2))
    assert(cons(1, cons(2, cons(3, Empty))).takeWhile(x => x < 4).toList == List(1, 2, 3))
    assert(cons(1, cons(2, cons(3, Empty))).takeWhile(x => x < 0).toList == Nil)
  }
}

object TestForAll {

  def raise(): Int = {
    sys.error("raise")
  }

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, Empty)).forAll(3 >= _))
    assert(!cons(1, cons(2, cons(raise(), Empty))).forAll(1 >= _))
  }
}

object TestHeadOption {

  def main(args: Array[String]): Unit = {
    assert(Empty.headOption.isEmpty)
    assert(cons(1, Empty).headOption.contains(1))
    assert(cons(1, cons(2, Empty)).headOption.contains(1))
  }
}

object TestMap {

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, cons(3, Empty))).map(_.toString).toList == List("1", "2", "3"))
  }
}

object TestFilter {

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, cons(3, Empty))).filter(_ % 2 == 0).toList == List(2))
    assert(cons(1, cons(2, cons(3, Empty))).filter(_ % 2 == 1).toList == List(1, 3))
    assert(cons(1, cons(2, cons(3, Empty))).filter(_ > 5).toList == List())
  }
}

object TestAppend {

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, cons(3, Empty))).append(cons(4, cons(5, Empty))).toList == List(1, 2, 3, 4, 5))
    assert(Empty.append(cons(4, cons(5, Empty))).toList == List(4, 5))
    assert(cons(1, cons(2, cons(3, Empty))).append(Empty).toList == List(1, 2, 3))
  }
}

object TestFlatMap {

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, cons(3, Empty))).flatMap(x => cons(x, cons(x, Empty))).toList == List(1, 1, 2, 2, 3, 3))
    assert(Empty.flatMap(x => cons(x, cons(x, Empty))).toList == List())
    assert(cons(1, cons(2, cons(3, Empty))).flatMap(x => Empty).toList == List())
  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] = cons(a, f(b, a + b))
    f(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(t => cons(t._1, unfold(t._2)(f))).getOrElse[Stream[A]](Empty)
  }
}

object TestFrom {

  import Stream.from

  def main(args: Array[String]): Unit = {
    assert(from(5).take(3).toList == List(5, 6, 7))
    assert(from(-1).take(1).toList == List(-1))
  }

}

object TestFib {

  import Stream.fibs

  def main(args: Array[String]): Unit = {
    assert(fibs.take(1).toList == List(0))
    assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }
}

object TestUnfold {

  import Stream.unfold

  def main(args: Array[String]): Unit = {
    assert(unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2))).take(1).toList == List(0))
    assert(unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2))).take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }
}