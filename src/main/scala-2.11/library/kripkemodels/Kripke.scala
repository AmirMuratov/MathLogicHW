package library.kripkemodels

import library.propositionalcalculus.{Expression, PropVar}

import scala.collection.mutable
/**
  * Created by amir on 10.02.17.
  */
object Kripke {
  def findModel(expression: Expression): Option[Model] = {
    val worlds = generateWorlds(expression)
    val model = new Model(worlds.head)
    val allModels = generateModel(model, worlds, new mutable.ArrayBuffer[Model])
    if (checkAllModels(expression, 0, allModels, model)) {
      None
    } else {
      Some(model)
    }
  }

  def checkAllModels(expr: Expression, index: Int, allModels: Seq[Model], mainModel: Model): Boolean = {
    val model: Model = allModels(index)
    if (model eq allModels.last) {
      model.setActive(true)
      if (!mainModel.check(expr)) {
        return false
      }
      model.setActive(false)
      return mainModel.check(expr)
    }
    model.setActive(false)
    var next = index + 1
    if (model.getSubtree != 0) {
      next = index + model.getSubtree
    }
    if (!checkAllModels(expr, next, allModels, mainModel)) {
      return false
    }
    model.setActive(true)
    checkAllModels(expr, index + 1, allModels, mainModel)
  }

  def generateWorlds(expression: Expression): Seq[World] = {
    val variables = expression.getVariables.toSeq.map(PropVar)
    val worlds = new mutable.ArrayBuffer[World]()
    var i: Long = 0
    while (i < (1 << variables.size)) {
      {
        val world: World = new World()
        var j: Int = 0
        while (j < variables.size) {
          {
            if ((i & (1 << j)) != 0) {
              world.forceVariable(variables(j))
            }
          }
          j += 1
        }
        worlds += world
      }
      i += 1
    }
    worlds
  }

  def generateModel(model: Model, worlds: Seq[World], allModels: mutable.ArrayBuffer[Model]): Seq[Model] = {
    worlds.filter(world => model.getWorld.isLesserSubset(world)).foreach((world: World) => {
      model.addChild(world)
      val toAdd = model.getChildren(model.getChildren.size - 1)
      allModels += toAdd
      generateModel(toAdd, worlds, allModels)
      model.setSubtree(model.getSubtree + toAdd.getSubtree + 1)
    })
    allModels
  }
}
