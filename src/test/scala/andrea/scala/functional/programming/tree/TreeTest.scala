package andrea.scala.functional.programming.tree

/**
  * Created by andrea on 10/2/16.
  */
object TreeTest extends App {

  val singleLeaf = Leaf(1)
  val unevenTree = Branch(
    Branch(Leaf(1), Leaf(6)),
    Branch(
      Branch(Leaf(3), Leaf(4)),
      Leaf(5)))
  val stringTree = Branch(
    Branch(Leaf("aa"), Leaf("ff")),
    Branch(
      Branch(Leaf("cc"), Leaf("dd")),
      Leaf("ee")
    )
  )


  println("Test size")
  assert(singleLeaf.size == 1)
  assert(unevenTree.size == 9)
  assert(stringTree.size == 9)

  println("Test maximum")
  assert(singleLeaf.maximum == 1)
  assert(unevenTree.maximum == 6)

  println("Test depth")
  assert(singleLeaf.depth == 0)
  assert(unevenTree.depth == 3)
  assert(stringTree.depth == 3)

  println("Test map")
  assert(singleLeaf.map(_ * 2) == Leaf(2))
  val doubleUnevenTree = Branch(
    Branch(Leaf(2), Leaf(12)),
    Branch(
      Branch(Leaf(6), Leaf(8)),
      Leaf(10)))
  assert(unevenTree.map(_ * 2) == doubleUnevenTree)
  val unevenTreeAllTwos = Branch(
    Branch(Leaf(2), Leaf(2)),
    Branch(
      Branch(Leaf(2), Leaf(2)),
      Leaf(2)))
  assert(stringTree.map(_.length) == unevenTreeAllTwos)

}
