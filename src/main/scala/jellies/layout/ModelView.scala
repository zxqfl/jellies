package jellies.layout

import jellies.game

sealed trait ModelView {
  val model: game.Model
  val player: model.PlayerHandle
  
  def perspective = model.perspectives(player)
}

object ModelView {
  def apply(m: game.Model)(p: m.PlayerHandle): ModelView = {
    new ModelView {
      val model: m.type = m
      val player = p
    }
  }
}