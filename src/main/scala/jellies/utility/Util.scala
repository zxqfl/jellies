package jellies.utility

object Util {
  def invert[A, B](original: Map[A, B]): Map[B, Set[A]] = {
    var result: Map[B, Set[A]] = Map()
    for ((k, v) <- original) {
      val crnt = result.getOrElse(v, Set())
      result += v -> (crnt + k)
    }
    result
  }
}