package jellies.client

import jellies.game.LevelSpecification
import jellies.game.Model
import jellies.layout.ModelView
import jellies.game.Location
import jellies.game.Direction
import jellies.layout.Layout
import jellies.game.metadata.FollowedBy
import jellies.game.DirectionBlocked
import jellies.game.JellyPermissionFailure

class GameStateManager(val canvasManager: CanvasManager) {
  private var optModel: Option[Model] = None
  canvasManager.addRedrawListener(onRedraw)
  
  def setLevel(specification: LevelSpecification): Unit = {
    val model = new Model(specification)
    optModel = Some(model)
    val views = for {
      h <- model.playerHandles
    } yield ModelView(model)(h)
    canvasManager.setModelView(views: _*)
  }
  
  def goToNextLevel(): Either[Unit, Unit] = {
    optModel match {
      case None => Left(Unit)
      case Some(model) => {
        val optNext = model.metadata.find(_.isInstanceOf[FollowedBy])
        optNext match {
          case Some(FollowedBy(level)) => {
            setLevel(level)
            Right(Unit)
          }
          case _ => {
            clearModel()
            Left(Unit)
          }
        }
      }
    }
  }
  
  def clearModel() = {
    optModel = None
    canvasManager.setModelView()
    onModelUpdate()
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
        case Left(DirectionBlocked) => {
          canvasManager.showMessage("That direction is blocked.")
        }
        case Left(JellyPermissionFailure) => {
          canvasManager.showMessage("That jelly is in the air.")
        }
        case Left(_) =>
        case Right(result) => {
          canvasManager.animateMove(result)
          onModelUpdate()
        }
      }
    }
  }
  
  def restartLevel(): Unit = {
    if (optModel.isDefined) {
      canvasManager.cancelAnimation()
      optModel.get.restart()
      onModelUpdate()
    }
  }
  
  def undoMove(): Unit = {
    if (optModel.isDefined && optModel.get.canUndo) {
      canvasManager.cancelAnimation()
      optModel.get.undo()
      onModelUpdate()      
    }
  }
  
  private def onModelUpdate(): Unit = {
    canvasManager.redraw()
  }
  
  private def onRedraw(): Unit = {
    if (optModel.isDefined && optModel.get.isLevelSolved &&
        !canvasManager.isAnimationRunning) {
      goToNextLevel()
      canvasManager.showMessage("Level complete!")
    }
  }
}