package jellies.layout

import jellies.game

final case class ModelView(
    model: game.Model,
    player: game.PlayerHandle) {
  require(model.perspectives contains player)
  
  def perspective = model.perspectives(player)
}