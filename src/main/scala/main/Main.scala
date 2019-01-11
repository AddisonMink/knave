package main

import org.scalajs.dom
import dom.document

object Main extends App {
  val map = document.getElementById("map")
  map.innerHTML = "Hello, Knave!"
}
