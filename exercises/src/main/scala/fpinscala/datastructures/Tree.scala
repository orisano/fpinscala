package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => depth(l).max(depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: (A) => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(x) => l(x)
    case Branch(x, y) => b(fold(x)(l)(b), fold(y)(l)(b))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)((a, b) => a + b + 1)

  def maximum2(t: Tree[Int]): Int = fold(t)(x => x)((a, b) => a.max(b))

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 1)((a, b) => a.max(b) + 1)

  def map2[A, B](t: Tree[A])(f: (A) => B): Tree[B] = fold[A, Tree[B]](t)(x => Leaf(f(x)))((a, b) => Branch(a, b))
}

object TestSize {

  import Tree.size

  def main(args: Array[String]): Unit = {
    assert(size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)
    assert(size(Leaf(1)) == 1)
  }
}

object TestMaximum {

  import Tree.maximum

  def main(args: Array[String]): Unit = {
    assert(maximum(Leaf(100)) == 100)
    assert(maximum(Branch(Leaf(100), Branch(Leaf(10), Leaf(10000)))) == 10000)
    assert(maximum(Branch(Leaf(5), Leaf(10))) == 10)
  }
}

object TestDepth {

  import Tree.depth

  def main(args: Array[String]): Unit = {
    assert(depth(Leaf(1)) == 1)
    assert(depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    assert(depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 3)
    assert(depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))))) == 5)
  }
}

object TestMap {

  import Tree.map

  def main(args: Array[String]): Unit = {
    assert(map(Leaf(1))(x => x.toString) == Leaf("1"))
    assert(map(Branch(Leaf(1), Leaf(2)))(2.*) == Branch(Leaf(2), Leaf(4)))
    assert(map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(4.*) == Branch(Leaf(4), Branch(Leaf(8), Leaf(12))))
  }
}

object TestFold {

  import Tree.size2
  import Tree.maximum2
  import Tree.depth2
  import Tree.map2

  def main(args: Array[String]): Unit = {
    assert(size2(Branch(Leaf(1), Leaf(2))) == 3)
    assert(size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)
    assert(size2(Leaf(1)) == 1)

    assert(maximum2(Leaf(100)) == 100)
    assert(maximum2(Branch(Leaf(100), Branch(Leaf(10), Leaf(10000)))) == 10000)
    assert(maximum2(Branch(Leaf(5), Leaf(10))) == 10)

    assert(depth2(Leaf(1)) == 1)
    assert(depth2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
    assert(depth2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 3)
    assert(depth2(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))))) == 5)

    assert(map2(Leaf(1))(x => x.toString) == Leaf("1"))
    assert(map2(Branch(Leaf(1), Leaf(2)))(2.*) == Branch(Leaf(2), Leaf(4)))
    assert(map2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(4.*) == Branch(Leaf(4), Branch(Leaf(8), Leaf(12))))
  }
}