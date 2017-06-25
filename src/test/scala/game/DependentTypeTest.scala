package game

import org.scalatest._

private class DependentTypeTest extends FunSuite with Matchers {
  val example =
    """
    ......
    .22...
    .XX1..
    ...X1.
    ....X.
    """
  
  test("jelly references") {
    val a = State.fromASCIIArt(example)
    val b = State.fromASCIIArt(example)
    "a.jellyParts(a.selectByColour(MergeableColour(2)))" should compile
    "a.jellyParts(b.selectByColour(MergeableColour(2)))" shouldNot compile
  }
}