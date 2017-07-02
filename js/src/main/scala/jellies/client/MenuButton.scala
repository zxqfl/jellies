package jellies.client

trait MenuButton {
    def text: String
    def isVisible: Boolean
    def isClickable: Boolean
    def onClick(): Unit
}