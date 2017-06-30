package jellies.client

sealed trait TextAlign

case object AlignLeft extends TextAlign
case object AlignCentre extends TextAlign
case object AlignRight extends TextAlign