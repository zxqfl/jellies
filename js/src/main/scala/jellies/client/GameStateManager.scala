package jellies.client

import jellies.game.LevelSpecification
import jellies.game.Model
import jellies.layout.ModelView
import jellies.game.Location
import jellies.game.Direction

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
      view: ModelView,
      location: Location,
      direction: Direction): Unit = {
    if (optModel.isEmpty || (view.model ne optModel.get)) {
      System.err.println("An out of date model was submitted.")
    } else {
      val result = view.model.attemptMove(view.player, location, direction)
      result match {
        case Left(x) => canvasManager.showMessage(x.toString)
        case Right(_) => onModelUpdate()
      }
    }
  }
  
  private def onModelUpdate(): Unit = {
    canvasManager.redraw()
  }
}