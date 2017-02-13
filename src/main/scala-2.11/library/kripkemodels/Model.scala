package library.kripkemodels

import library.propositionalcalculus._

import scala.collection.mutable

/**
  * Created by amir on 10.02.17.
  */
class Model(var world: World) {
  private val children = new mutable.ArrayBuffer[Model]()
  private var active = true
  private var subtree = 0

  def addChild(world: World) {
    children += new Model(world)
  }

  private def checkImpl(x: Expression, y: Expression): Boolean = {
    if (check(x) && !check(y)) return false
    children.forall((child) => !child.isActive || child.checkImpl(x, y))
  }

  def check(expression: Expression): Boolean = {
    if (!active) return true
    expression match {
      case Disjunction(x, y) =>
        check(x) || check(y)
      case Conjunction(x, y) =>
        check(x) && check(y)
      case Implication(x, y) =>
        checkImpl(x, y)
      case Negate(x) =>
        if (check(x)) return false
        children.forall((child) => !child.isActive || !child.check(x))
      case a@PropVar(_) =>
        world.isForced(a)
      case _ => throw new IllegalStateException()
    }
  }

  def isActive: Boolean = active

  def setActive(active: Boolean) {
    this.active = active
  }

  def getWorld: World = world

  def getChildren = children

  def getSubtree: Int = subtree

  def setSubtree(subtree: Int) {
    this.subtree = subtree
  }

  def toString(sb: StringBuilder, pref: String, isLast: Boolean): Unit = {
    sb.append(pref)
    if (isLast)
      sb.append("┗")
    else
      sb.append("┣")
    sb.append("* ")
    sb.append(world.getVariables.mkString(" "))
    sb.append("\n")
    val s = children.count(_.isActive) - 1
    children.filter(_.isActive).zipWithIndex.foreach((x) => {
      x._1.toString(sb, if (isLast) pref + " " else pref + "┃",
        isLast = s == x._2)})
  }

  override def toString: String = {
    val sb = new StringBuilder
    toString(sb, "", isLast = true)
    sb.toString
  }
}

