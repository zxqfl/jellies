package jellies.client

import jellies.game.LevelSpecification
import jellies.game.Model
import jellies.layout.ModelView

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
  
  private def onModelUpdate(): Unit = {
    canvasManager.redraw()
  }
}