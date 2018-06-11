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

  def map2[B](f: (A) => B): Stream[B] = Stream.unfold(this) {
    case Cons(hd, tl) => Some((f(hd()), tl()))
    case Empty => None
  }

  def take2(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(hd, tl), x) if x > 0 => Some((hd(), (tl(), x - 1)))
    case _ => None
  }

  def takeWhile2(p: (A) => Boolean): Stream[A] = unfold(this) {
    case Cons(hd, tl) if p(hd()) => Some(hd(), tl())
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
    case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
    case (Cons(ah, at), Cons(bh, bt)) => Some(((Some(ah()), Some(bh())), (at(), bt())))
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll {
    case (a, b) => a == b
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s.drop(1)))
  }.append(Stream(Empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z)))((a, p0) => {
    lazy val p1 = p0
    val b2 = f(a, p1._1)
    (b2, cons(b2, p1._2))
  })._2
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object TestTake {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3).take(0).toList == Nil)
    assert(Stream(1, 2, 3).take(4).toList == List(1, 2, 3))
  }
}

object TestDrop {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).drop(2).toList == List(3))
    assert(Stream(1, 2, 3).drop(0).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).drop(4).toList == Nil)
  }
}

object TestTakeWhile {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).takeWhile(x => x < 3).toList == List(1, 2))
    assert(Stream(1, 2, 3).takeWhile(x => x < 4).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).takeWhile(x => x < 0).toList == Nil)
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
    assert(Stream(1).headOption.contains(1))
    assert(Stream(1, 2).headOption.contains(1))
  }
}

object TestMap {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).map(_.toString).toList == List("1", "2", "3"))
  }
}

object TestFilter {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).filter(_ % 2 == 0).toList == List(2))
    assert(Stream(1, 2, 3).filter(_ % 2 == 1).toList == List(1, 3))
    assert(Stream(1, 2, 3).filter(_ > 5).toList == Nil)
  }
}

object TestAppend {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).append(Stream(4, 5)).toList == List(1, 2, 3, 4, 5))
    assert(Empty.append(Stream(4, 5)).toList == List(4, 5))
    assert(Stream(1, 2, 3).append(Empty).toList == List(1, 2, 3))
  }
}

object TestFlatMap {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList == List(1, 1, 2, 2, 3, 3))
    assert(Empty.flatMap(x => Stream(x, x)).toList == Nil)
    assert(Stream(1, 2, 3).flatMap(x => Empty).toList == Nil)
  }
}

object TestMap2 {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).map2(_.toString).toList == List("1", "2", "3"))
    assert(Stream(1, 2, 3).map2(_ * 2).toList == List(2, 4, 6))
  }
}

object TestTake2 {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).take2(2).toList == List(1, 2))
    assert(Stream(1, 2, 3).take2(0).toList == Nil)
    assert(Stream(1, 2, 3).take2(4).toList == List(1, 2, 3))
  }
}

object TestTakeWhile2 {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).takeWhile2(x => x < 3).toList == List(1, 2))
    assert(Stream(1, 2, 3).takeWhile2(x => x < 4).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).takeWhile2(x => x < 0).toList == Nil)
  }
}

object TestZipWith {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).zipWith(Stream(4, 6, 8))(_ + _).toList == List(5, 8, 11))
    assert(Stream(1, 2, 3).zipWith(Stream(4, 6))(_ + _).toList == List(5, 8))
    assert(Stream(1, 2).zipWith(Stream(4, 6, 8))(_ + _).toList == List(5, 8))
  }
}

object TestZipAll {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).zipAll(Stream(4, 6, 8)).toList == List((Some(1), Some(4)), (Some(2), Some(6)), (Some(3), Some(8))))
    assert(Stream(1, 2).zipAll(Stream(4, 6, 8)).toList == List((Some(1), Some(4)), (Some(2), Some(6)), (None, Some(8))))
    assert(Stream(1, 2, 3).zipAll(Stream(4, 6)).toList == List((Some(1), Some(4)), (Some(2), Some(6)), (Some(3), None)))
  }
}

object TestStartsWith {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3) startsWith Stream(1, 2))
    assert(!Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)))
    assert(!Stream(1, 2, 3).startsWith(Stream(1, 1)))
  }
}

object TestTails {

  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).tails.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }
}

object TestScanRight {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
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

  def fibs2: Stream[Int] = unfold((0, 1)) {
    case (x, y) => Some(x, (y, x + y))
  }

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))
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

object TestFib2 {

  import Stream.fibs2

  def main(args: Array[String]): Unit = {
    assert(fibs2.take(1).toList == List(0))
    assert(fibs2.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }
}

object TestFrom2 {

  import Stream.from2

  def main(args: Array[String]): Unit = {
    assert(from2(5).take(3).toList == List(5, 6, 7))
    assert(from2(-1).take(1).toList == List(-1))
  }
}

object TestConstant2 {

  import Stream.constant2

  def main(args: Array[String]): Unit = {
    assert(constant2(1).take(1).toList == List(1))
    assert(constant2("a").take(5).toList == List("a", "a", "a", "a", "a"))
  }
}

object TestOnes2 {

  import Stream.ones2

  def main(args: Array[String]): Unit = {
    assert(ones2.take(1).toList == List(1))
    assert(ones2.take(5).toList == List(1, 1, 1, 1, 1))
  }
}