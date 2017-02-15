package library.kripkemodels

import scala.collection.mutable

/**
  * Created by amir.
  */
class World {
  var from = 0
  var to = new mutable.HashSet[Int]()
  var forced = 0

  def isForced(variableNumber: Int): Boolean = {
    (forced >> variableNumber) % 2 == 1
  }

  def addTo(toWorld: Int) {
    to.add(toWorld)
  }
}

object World {
  def createWorld(from: Int): World = {
    val res = new World()
    res.from = from
    res
  }
}
