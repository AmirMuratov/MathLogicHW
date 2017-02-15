package library.kripkemodels

import library.propositionalcalculus._

import scala.collection.mutable
/**
  * Created by amir.
  */
class Kripke() {
  private var worldsNumber = 0
  private var cur: Model = _
  private var variables: Set[String] = _
  private val variableToId = new mutable.HashMap[String, Integer]()
  private var expression: Expression = _
  private var worldToExpressions: mutable.HashMap[Integer, mutable.Map[Expression, Boolean]] = _
  private var modelsCount = 0

  private def getValue(world: Int, expr: Expression): Boolean = {
    if (worldToExpressions.contains(world) && worldToExpressions.get(world).contains(expr))
      return worldToExpressions(world)(expr)
    var res = false
    expr match {
      case PropVar(name) =>
        res = cur.isForcedInWorld(world, variableToId(name))
      case Disjunction(x, y) =>
        res = getValue(world, x) || getValue(world, y)
      case Conjunction(x, y) =>
        res = getValue(world, x) && getValue(world, y)
      case Negate(x) =>
        res = !getValue(world, x)
        val toEdges = cur.getWorld(world).to
        for (toEdge <- toEdges) {
          res &= getValue(toEdge, expr)
        }
      case Implication(x, y) =>
        res = !getValue(world, x) || getValue(world, y)
        val toEdges = cur.getWorld(world).to
        for (toEdge <- toEdges) {
          res &= getValue(toEdge, expr)
        }
      case _ => throw new IllegalStateException()
    }
    if (!worldToExpressions.contains(world))
      worldToExpressions.put(world, new mutable.HashMap[Expression, Boolean]())
    worldToExpressions(world).put(expr, res)
    worldToExpressions(world)(expr)
  }

  private def buildVariables(x: Int): Model = {
    if (x == worldsNumber) {
      worldToExpressions = new mutable.HashMap[Integer, mutable.Map[Expression, Boolean]]()
      if (!getValue(0, expression)) return cur
      null
    }
    else {
      val forcedBefore = cur.getForcedForWorld(x)
      var newForced = 0
      if (cur.getWorld(x).from != -1) newForced = cur.getForcedForWorld(cur.getWorld(x).from)
      else newForced = forcedBefore
      var notForcedVariables = 0
      val notForcedNumbers = new mutable.ArrayBuffer[Integer]
      var i = 0
      while (i < cur.variablesNumber) {
        if ((newForced >> i) % 2 == 0) {
          notForcedVariables += 1
          notForcedNumbers += i
        }
        i += 1
      }
      if (notForcedVariables == 0) return null
      var variableMask = 0
      while (variableMask < (1 << notForcedVariables)) {
        val nf = newForced
        var i = 0
        while (i < notForcedVariables) {
          newForced |= (((variableMask >> i) % 2) << notForcedNumbers(i))
          i += 1
        }
        cur.setForced(x, newForced)
        val res = buildVariables(x + 1)
        if (res != null) return res
        cur.setForced(x, forcedBefore)
        newForced = nf
        variableMask += 1
      }
      null
    }
  }

  private def buildWorlds(x: Int): Model = {
    if (x == worldsNumber) {
      modelsCount += 1
      buildVariables(0)
    }
    else {
      if (x == 0) {
        cur.addWorld(-1)
        val ans = buildWorlds(x + 1)
        if (ans != null) return ans
        cur.removeLastWorld()
        return null
      }
      var from = 0
      while (from < x) {
        cur.addWorld(from)
        val ans = buildWorlds(x + 1)
        if (ans != null) return ans
        cur.removeLastWorld()
        from += 1
      }
      null
    }
  }

  def findModel(expr: Expression): Option[Model] = {
    println(s"Max worlds: ${Kripke.MAX_WORLDS}")
    expression = expr
    variables = expr.getVariables
    val variablesList = variables.toSeq
    val variableIdToName = new mutable.HashMap[Int, String]
    var i = 0
    while (i < variables.size) {
      variableToId.put(variablesList(i), i)
      variableIdToName.put(i, variablesList(i))
      i += 1
    }
    worldsNumber = 1
    while (worldsNumber <= Kripke.MAX_WORLDS) {
      println(s"numOfWorlds: $worldsNumber")
      modelsCount = 0
      cur = Model.createModel(variables.size, variableIdToName)
      val m = buildWorlds(0)
      if (m != null) return Some(m)
      worldsNumber += 1
    }
    None
  }
}

object Kripke {
  val MAX_WORLDS: Int = 8
}

