package library.kripkemodels


import library.propositionalcalculus._

import scala.collection.mutable

/**
  * Created by amir on 10.02.17.
  */
class World {
  private val variables = new mutable.HashSet[Expression]

  def forceVariable(variable: Expression) {
    variables.add(variable)
  }

  def isForced(variable: Expression) = variables.contains(variable)

  def isLesserSubset(other: World): Boolean =
    variables.subsetOf(other.variables) && !other.variables.subsetOf(variables)

  def getVariables = variables

  override def toString: String = "World{" + "variables=" + variables + '}'
}
