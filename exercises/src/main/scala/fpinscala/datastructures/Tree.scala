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