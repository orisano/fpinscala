package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
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