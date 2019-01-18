package knave.game

sealed trait Speed
case object Normal extends Speed
case object Slow extends Speed
case object Fast extends Speed