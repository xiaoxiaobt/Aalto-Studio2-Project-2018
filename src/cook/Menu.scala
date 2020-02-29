package cook

import collection.mutable.Map

class Menu {

  var fridge = new Fridge()
  var settings = Settings
  var menu = getMenu()
  var testList = fridge.foodList.clone()
  var testState = true

  def p[T](a: T) = if (settings.diagnosis) println(a.toString)

  def menuFoodlist = fridge.foodList.filter(_._1.isMenu)

  def nonMenuFoodlist = fridge.foodList.filter(!_._1.isMenu)

  def getMenu() = fridge.foodList.filter(_._1.isMenu).map(_._1).toVector

  def addMenu(food: Food): Boolean = {
    if (fridge.foodList.contains(food)) {
      food.setToMenu()
      true
    } else {
      false
    }
  }

  def deleteMenu(food: Food): Boolean = {
    if (fridge.foodList.contains(food)) {
      food.setToRaw()
      true
    } else {
      false
    }
  }

  def exisitingAmount(food: Food): Double = fridge.foodList(food)

  //def initialize(): Unit = {
  //  test_list = fridge.food_list ++ Map()
  //  test_state = true
  //}

  /**Returns the amount of food as an integer*/
  def checkAvailability(food: Food): Int = {
    testList = fridge.foodList ++ Map()
    testState = true
    var i = 0
    if (fridge.foodList.map(_._1).toList.contains(food)) {
      while (testState) {
        checkAmount(food, 1)
        if (testState) i += 1
      }
    }
    i
  }

  def checkAmount(food: Food, num: Double): Unit = {
    if (fridge.foodList.keys.toArray.contains(food)) {
      var current_amount = testList(food)
      if (current_amount >= num) {
        testList = testList updated (food, testList(food) - num)
      } else if (current_amount > 0) {
        testList = testList updated (food, 0)
        checkAmount(food, num - current_amount)
      } else {
        if (food.ingredients.isEmpty) {
          testState = false
        } else {
          food.ingredients.foreach(x => checkAmount(x._1, x._2))
        }
      }
    } else {
      testState = false
    }
  }

  def returnFoodWithName(name: String): Option[Food] = {
    var result = fridge.foodList.find(_._1.name == name)
    if (result.isDefined) Some(result.get._1) else None
  }
  def allIngredientsExist(names: Iterable[String]): Boolean = {
    var result = names.map(x => returnFoodWithName(x))
    if (result.forall(_.isDefined)) true else false
  }

  def makeDish(food: Food, num: Double): Unit = {
    if (fridge.foodList(food) >= num) {
      fridge.foodList(food) -= num
    } else if (fridge.foodList(food) > 0) {
      var temp = num - fridge.foodList(food)
      fridge.foodList(food) = 0
      makeDish(food, temp)
    } else {
      food.ingredients.foreach(x => makeDish(x._1, x._2 * num))
    }
  }

}