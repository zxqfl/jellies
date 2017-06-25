package jellies.client

import scala.scalajs.js.JSApp
import org.scalajs.dom
import jellies.game
import jellies.layout

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
    
    CanvasManager.init(canvas)
    
    val model = new game.Model(game.ExampleLevels.multiplayerLevel)
    val player = game.ExampleLevels.reader
    CanvasManager.setModelView(
        layout.ModelView(model, player),
        layout.ModelView(model, game.ExampleLevels.other))
	}
}