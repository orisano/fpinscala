package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, tail) => tail
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, tail) => Cons(h, tail)
    case Nil => Nil
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Cons(_, tail) => drop(tail, n - 1)
      case Nil => Nil
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) =>
      if (f(head)) dropWhile(tail, f)
      else l
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
    case Nil => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((a, b) => f(b, a))
  }

  def append2[A](a: List[A], b: List[A]): List[A] = reverse(foldLeft(b, reverse(a))((acc, x) => Cons(x, acc)))

  def flatten[A](ls: List[List[A]]): List[A] = foldRight(ls, List[A]())(append)

  def plusOne(ints: List[Int]): List[Int] = foldRight(ints, List[Int]())((x, acc) => Cons(x + 1, acc))

  def doublesToString(doubles: List[Double]): List[String] = foldRight(doubles, List[String]())((x, acc) => Cons(x.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((x, acc) => if (f(x)) Cons(x, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

  def listAdd(as: List[Int], bs: List[Int]): List[Int] = {
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, at), Cons(b, bt)) => Cons(a + b, listAdd(at, bt))
    }
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, at), Cons(b, bt)) => Cons(f(a, b), zipWith(at, bt)(f))
    }
  }

  def startsWith[A](as: List[A], bs: List[A]): Boolean = {
    (as, bs) match {
      case (_, Nil) => true
      case (Cons(a, at), Cons(b, bt)) if a == b => startsWith(at, bt)
      case _ => false
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(a, as), Cons(b, bs)) if a == b && startsWith(as, bs) => true
      case (Cons(_, as), bs) => hasSubsequence(as, bs)
    }
  }
}

object TestX {

  import List.x

  def main(args: Array[String]): Unit = {
    assert(x == 3)
  }
}

object TestTail {

  import List.tail

  def main(args: Array[String]): Unit = {
    assert(tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
    assert(tail(List(1)) == Nil)
    assert(tail(List()) == Nil)
  }
}

object TestSetHead {

  import List.setHead

  def main(args: Array[String]): Unit = {
    assert(setHead(List(1, 2, 3), 5) == List(5, 2, 3))
    assert(setHead(List(2), 4) == List(4))
    assert(setHead(Nil, "foo") == Nil)
  }
}

object TestDrop {

  import List.drop

  def main(args: Array[String]): Unit = {
    assert(drop(List(1, 2, 3, 4, 5), 0) == List(1, 2, 3, 4, 5))
    assert(drop(List(1, 2, 3, 4, 5), 1) == List(2, 3, 4, 5))
    assert(drop(List(1, 2, 3, 4, 5), 3) == List(4, 5))
    assert(drop(List(1, 2, 3, 4, 5), 6) == Nil)
    assert(drop(Nil, 3) == Nil)
  }
}

object TestDropWhile {

  import List.dropWhile

  def main(args: Array[String]): Unit = {
    assert(dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3) == List(3, 4, 5))
    assert(dropWhile(List(1, 2, 3, 2, 5), (x: Int) => x < 3) == List(3, 2, 5))
    assert(dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 6) == Nil)
    assert(dropWhile(Nil, (x: Int) => x < 6) == Nil)
  }
}

object TestInit {

  import List.init

  def main(args: Array[String]): Unit = {
    assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))
    assert(init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
    assert(init(List(4)) == Nil)
    assert(init(Nil) == Nil)
  }
}

/*
Exercise 3.7
foldRightを使って実装されたproductは0.0を検出しても直ちに中止することは出来ない.
fにおける零元がある場合のみ零元を返す実装は可能だと思われる.
 */

object TestLength {

  import List.length

  def main(args: Array[String]): Unit = {
    assert(length(List(1, 2, 3, 4)) == 4)
    assert(length(List(1)) == 1)
    assert(length(List()) == 0)
  }
}

object TestFoldLeft {

  import List.foldLeft

  def main(args: Array[String]): Unit = {
    assert(foldLeft(List(1, 2, 3, 4, 5), 1)(_ * _) == 120)
    assert(foldLeft(Nil: List[Int], 1)(_ * _) == 1)
    assert(foldLeft(List(5), 1)(_ * _) == 5)
    assert(foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
    assert(foldLeft(Nil: List[Int], 0)(_ + _) == 0)
    assert(foldLeft(List(5), 0)(_ + _) == 5)
    assert(foldLeft(List(1, 2, 3, 4), List[Int]())((acc, x) => Cons(x, acc)) == List(4, 3, 2, 1))
  }
}

object Exercise3_11 {

  import List.foldLeft

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def main(args: Array[String]): Unit = {
    // sum
    assert(sum(List(1, 2, 3, 4, 5, 6)) == 21)
    assert(sum(List()) == 0)
    assert(sum(List(15)) == 15)

    // product
    assert(product(List(1.0, 2.0, 3.0, 4.0)) == 24.0)
    assert(product(List(1.0, 0.0, 3.0, 4.0)) == 0.0)
    assert(product(List()) == 1.0)

    // length
    assert(length(List()) == 0)
    assert(length(List("hello")) == 1)
    assert(length(List("foo", "bar")) == 2)
  }
}

object TestReverse {

  import List.reverse

  def main(args: Array[String]): Unit = {
    assert(reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
    assert(reverse(List(1)) == List(1))
    assert(reverse(Nil) == Nil)
  }
}

object TestFoldLeft2 {

  import List.foldLeft2

  def main(args: Array[String]): Unit = {
    assert(foldLeft2(List(1, 2, 3, 4, 5), 1)(_ * _) == 120)
    assert(foldLeft2(Nil: List[Int], 1)(_ * _) == 1)
    assert(foldLeft2(List(5), 1)(_ * _) == 5)
    assert(foldLeft2(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
    assert(foldLeft2(Nil: List[Int], 0)(_ + _) == 0)
    assert(foldLeft2(List(5), 0)(_ + _) == 5)
    assert(foldLeft2(List(1, 2, 3, 4), List[Int]())((acc, x) => Cons(x, acc)) == List(4, 3, 2, 1))
  }
}

object TestFoldRight2 {

  import List.foldRight2

  def main(args: Array[String]): Unit = {
    assert(foldRight2(List(1, 2, 3, 4, 5), 1)(_ * _) == 120)
    assert(foldRight2(Nil: List[Int], 1)(_ * _) == 1)
    assert(foldRight2(List(5), 1)(_ * _) == 5)
    assert(foldRight2(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
    assert(foldRight2(Nil: List[Int], 0)(_ + _) == 0)
    assert(foldRight2(List(5), 0)(_ + _) == 5)
    assert(foldRight2(List(1, 2, 3, 4), List[Int]())(Cons[Int]) == List(1, 2, 3, 4))
  }
}

object TestAppend2 {

  import List.append2

  def main(args: Array[String]): Unit = {
    assert(append2(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
    assert(append2(List(1, 2, 3), List()) == List(1, 2, 3))
    assert(append2(List(), List(4, 5, 6)) == List(4, 5, 6))
  }
}

object TestFlatten {

  import List.flatten

  def main(args: Array[String]): Unit = {
    assert(flatten(List(List(1, 2, 3), List(4), List(5, 6))) == List(1, 2, 3, 4, 5, 6))
    assert(flatten(List(List(1, 2, 3), List[Int](), List(5, 6))) == List(1, 2, 3, 5, 6))
    assert(flatten(List(List(1, 2, 3), List(4), List[Int]())) == List(1, 2, 3, 4))
    assert(flatten(List(List[Int](), List(4), List(5, 6))) == List(4, 5, 6))
    assert(flatten(List(List(1, 2, 3))) == List(1, 2, 3))
    assert(flatten(List()) == Nil)
  }
}

object TestPlusOne {

  import List.plusOne

  def main(args: Array[String]): Unit = {
    assert(plusOne(List(1, 2, 3, 4)) == List(2, 3, 4, 5))
    assert(plusOne(List(1)) == List(2))
    assert(plusOne(List()) == List())
  }
}

object TestDoublesToString {

  import List.doublesToString

  def main(args: Array[String]): Unit = {
    assert(doublesToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
    assert(doublesToString(List(0.5)) == List("0.5"))
    assert(doublesToString(List()) == List())
  }
}

object TestMap {

  import List.map

  def main(args: Array[String]): Unit = {
    assert(map(List(1, 2, 3, 4, 5))(x => x + 1) == List(2, 3, 4, 5, 6))
    assert(map(List(1, 2, 3, 4, 5))(x => x.toString) == List("1", "2", "3", "4", "5"))
  }
}

object TestFilter {

  import List.filter

  def main(args: Array[String]): Unit = {
    assert(filter(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 0) == List(2, 4, 6))
    assert(filter(List[Int]())(x => x % 2 == 0) == List())
  }
}

object TestFlatMap {

  import List.flatMap

  def main(args: Array[String]): Unit = {
    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }
}

object TestFilter2 {

  import List.filter2

  def main(args: Array[String]): Unit = {
    assert(filter2(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 0) == List(2, 4, 6))
    assert(filter2(List[Int]())(x => x % 2 == 0) == List())
  }
}

object TestListAdd {

  import List.listAdd

  def main(args: Array[String]): Unit = {
    assert(listAdd(List(1, 2, 3, 4), List(2, 1, 5, 3)) == List(3, 3, 8, 7))
    assert(listAdd(List(), List(2, 1, 5, 3)) == List())
    assert(listAdd(List(1, 2, 3, 4), List()) == List())
    assert(listAdd(List(1, 2, 3, 4), List(10, 1)) == List(11, 3))
    assert(listAdd(List(1, 2), List(5, 6, 7)) == List(6, 8))
  }
}

object TestZipWith {

  import List.zipWith

  def main(args: Array[String]): Unit = {
    assert(zipWith(List(1, 2, 3, 4), List(2, 1, 5, 3))((a, b) => a + b) == List(3, 3, 8, 7))
    assert(zipWith(List[Int](), List(2, 1, 5, 3))((a, b) => a + b) == List())
    assert(zipWith(List(1, 2, 3, 4), List[Int]())((a, b) => a + b) == List())
    assert(zipWith(List(1, 2, 3, 4), List(10, 1))((a, b) => a + b) == List(11, 3))
    assert(zipWith(List(1, 2), List(5, 6, 7))((a, b) => a + b) == List(6, 8))

    assert(zipWith(List("1", "2"), List("5", "6"))((a, b) => a + b) == List("15", "26"))
    assert(zipWith(List(1, 2), List(5, 6, 7))((a, b) => a * b) == List(5, 12))
    assert(zipWith(List(1, 2), List("a", "b"))((a, b) => a.toString + b) == List("1a", "2b"))

  }
}

object TestHasSubsequence {

  import List.hasSubsequence

  def main(args: Array[String]): Unit = {
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(!hasSubsequence(List(1, 2, 3, 4), List(2, 4)))
  }
}