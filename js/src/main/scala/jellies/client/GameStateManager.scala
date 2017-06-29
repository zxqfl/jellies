package jellies.client

import jellies.game.LevelSpecification
import jellies.game.Model
import jellies.layout.ModelView
import jellies.game.Location
import jellies.game.Direction
import jellies.layout.Layout

class GameStateManager(val canvasManager: CanvasManager) {
  private var optModel: Option[Model] = None
  
  def setLevel(specification: LevelSpecification): Unit = {
    val model = new Model(specification)
    optModel = Some(model)
    val views = for {
      h <- model.playerHandles
    } yield ModelView(model)(h)
    canvasManager.setModelView(views: _*)
  }
  
  def submitMoveAttempt(
      layout: Layout,
      location: Location,
      direction: Direction): Unit = {
    if (optModel.isEmpty ||
        (layout.moveInfo.resultingState ne optModel.get.currentState.state)) {
      System.err.println("An out of date model was submitted.")
    } else {
      val model = optModel.get
      val result = model.attemptMove(
          model.playerWithPerspective(layout.perspective),
          location, direction)
      result match {
        case Left(x) => canvasManager.showMessage(x.toString)
        case Right(result) => {
          canvasManager.animateMove(result)
          onModelUpdate()
        }
      }
    }
  }
  
  private def onModelUpdate(): Unit = {
    canvasManager.redraw()
  }
}