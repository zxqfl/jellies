package jellies.client

import scala.scalajs.js.JSApp
import org.scalajs.dom
import jellies.game
import jellies.layout
import jellies.game.LevelSpecification
import jellies.game.levels.Levels

object EntryPoint extends JSApp {
  def main(): Unit = {
    val page = dom.document.getElementById("main")
    page.innerHTML = """
        <div>
            <canvas id="canvas" style="margin: 0;
                                       position: absolute;
                                       top: 0;
                                       left: 0;"/>
        </div>
        """
    val canvas = dom.document.getElementById("canvas").
        asInstanceOf[dom.html.Canvas]
    
    val canvasManager = new CanvasManager(canvas)
    val stateManager = new GameStateManager(canvasManager)
    stateManager.setLevel(Levels.levels(0))
    val inputManager = new UserInputManager(stateManager)
  }
}