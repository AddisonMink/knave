package knave.main

import org.scalajs.dom.document

object Main extends App {
  val map = document.getElementById("map")
  map.innerHTML = "Hello, Knave!"
}
