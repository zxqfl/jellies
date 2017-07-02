package jellies.client

trait MenuButton {
    def text: String
    def isVisible: Boolean
    def isClickable: Boolean
    def onClick(): Unit
}

object DummyMenuButton extends MenuButton {
  def text = ???
  def isVisible = false
  def isClickable = ???
  def onClick() = ???
}