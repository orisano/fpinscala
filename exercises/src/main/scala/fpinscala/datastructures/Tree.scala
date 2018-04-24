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