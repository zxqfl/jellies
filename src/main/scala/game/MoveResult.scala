package game
 
sealed trait MoveResult {
  val initialState: State
  val resultingState: State
  val effects: Seq[initialState.MoveEffect]
  val refMap: Map[initialState.JellyRef, resultingState.JellyRef]
}

private object MoveResult {
  def apply(
      is: State,
      rs: State)(
      e: Seq[is.MoveEffect],
      rm: Map[is.JellyRef, rs.JellyRef]) = {
    new MoveResult {
      val initialState: is.type = is
      val effects = e
      val resultingState: rs.type = rs
      val refMap = rm
    }
  }
}