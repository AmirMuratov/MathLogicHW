package library.kripkemodels

import scala.collection.mutable

/**
  * Created by amir.
  */
class Model {
  var variablesNumber = 0
  var variableIdToVariableName: mutable.HashMap[Int, String] = _
  val worlds = new mutable.ArrayBuffer[World]()

  def isForcedInWorld(worldNumber: Int, variableNumber: Int): Boolean =
    worlds(worldNumber).isForced(variableNumber)

  def getWorld(worldNumber: Int): World = worlds(worldNumber)

  def addWorld(from: Int) {
    worlds += World.createWorld(from)
    if (from != -1) addTo(from, worlds.size - 1)
  }

  def removeLastWorld() {
    worlds.remove(worlds.size - 1)
    for (world <- worlds) {
      world.to.remove(worlds.size)
    }
  }

  def getForcedForWorld(worldNumber: Int): Int = worlds(worldNumber).forced

  def setForced(worldNumber: Int, forced: Int) {
    worlds(worldNumber).forced = forced
  }

  def addTo(worldNumber: Int, toWorld: Int) {
    worlds(worldNumber).addTo(toWorld)
  }

  def print(world: Int, sb: StringBuilder, pref: String, isLast: Boolean): Unit = {
    sb.append(pref)
    if (isLast)
      sb.append("┗")
    else
      sb.append("┣")
    sb.append("*")
    val forced = worlds(world).forced
    var j = 0
    while (j < variablesNumber) {
      val forcedVariable = (forced >> j) % 2
      if (forcedVariable == 1) sb.append(" ").append(variableIdToVariableName(j))
      j += 1
    }
    sb.append("\n")
    val s = worlds(world).to.size - 1
    worlds(world).to.zipWithIndex.foreach((x) => {
      print(x._1, sb, if (isLast) pref + " " else pref + "┃",
        isLast = s == x._2)
    })
  }

  override def toString: String = {
    val sb = new mutable.StringBuilder()
    print(worlds.zipWithIndex.find(_._1.from == -1).get._2, sb, "", isLast = true)
    sb.toString()
  }
}

object Model {
  def createModel(variables: Int, variableIdToVariableName: mutable.HashMap[Int, String]): Model = {
    val res = new Model()
    res.variablesNumber = variables
    res.variableIdToVariableName = variableIdToVariableName
    res
  }
}
