package cook

import collection.mutable.Map

class Menu {

  var foodList = Map[Food, Double]()
  var menu = getMenu()
  var testList = foodList.clone()
  var testState = true

  def p[T](a: T) = if (Settings.diagnosis) println(a.toString)

  def foodListRaw = foodList.filter(_._1.ingredients.isEmpty)
  def foodListCooked = foodList.filter(_._1.ingredients.nonEmpty)

  def menuFoodlist = foodList.filter(_._1.isMenu)
  def nonMenuFoodlist = foodList.filter(!_._1.isMenu)
  def getMenu() = foodList.filter(_._1.isMenu).map(_._1).toVector

  def addMenu(food: Food): Boolean = {
    if (foodList.contains(food)) {
      food.setToMenu()
      true
    } else {
      false
    }
  }

  def deleteMenu(food: Food): Boolean = {
    if (foodList.contains(food)) {
      food.setToRaw()
      true
    } else {
      false
    }
  }

  def exisitingAmount(food: Food): Double = foodList(food)

  //def initialize(): Unit = {
  //  test_list = fridge.food_list ++ Map()
  //  test_state = true
  //}

  /** Returns the amount of food as an integer */
  def checkAvailability(food: Food): Int = {
    testList = foodList ++ Map()
    testState = true
    var i = 0
    if (foodList.map(_._1).toList.contains(food)) {
      while (testState) {
        checkAmount(food, 1)
        if (testState) i += 1
      }
    }
    i
  }

  def checkAmount(food: Food, num: Double): Unit = {
    if (foodList.keys.toArray.contains(food)) {
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
    var result = foodList.find(_._1.name == name)
    if (result.isDefined) Some(result.get._1) else None
  }
  
  def allIngredientsExist(names: Iterable[String]): Boolean = {
    var result = names.map(x => returnFoodWithName(x))
    if (result.forall(_.isDefined)) true else false
  }

  def makeDish(food: Food, num: Double): Unit = {
    if (foodList(food) >= num) {
      foodList(food) -= num
    } else if (foodList(food) > 0) {
      var temp = num - foodList(food)
      foodList(food) = 0
      makeDish(food, temp)
    } else {
      food.ingredients.foreach(x => makeDish(x._1, x._2 * num))
    }
  }

  def removeFood(food: Food, amount: Double): Boolean = {
    if (foodList.contains(food) && amount >= 0 && foodList(food) >= amount) {
      foodList = foodList updated (food, foodList(food) - amount)
      true
    } else {
      false
    }
  }

  def addFood(food: Food, amount: Double): Boolean = {
    if (amount > 0) {
      if (foodList.contains(food)) {
        foodList = foodList updated (food, foodList(food) + amount)
      } else {
        foodList += (food -> amount)
      }
      true
    } else {
      false
    }
  }
  
  def getByTags(tag: String): Map[Food, Double] = {
    var map = Map[Food, Double]()
    var tagList = tag.toUpperCase.trim.split("").distinct
    if (tag.trim.isEmpty)
      foodList
    else {
      for (item <- foodList.keys) {
        var uniqueTags = item.tag.toUpperCase.trim.split("").distinct
        if (tagList.intersect(uniqueTags).size == tagList.size) {
          map += (item -> foodList(item))
        }
      }
      map
    }
  }

  def getByName(name: String): Map[Food, Double] = {
    var map = Map[Food, Double]()
    var nameList = name.toUpperCase.trim
    for (item <- foodList.keys) {
      var item_name_list = item.name.toUpperCase.trim
      if (item_name_list matches ".*" + nameList + ".*") {
        map += (item -> foodList(item))
      }
    }
    map
  }

  def getByIngredients(name: String): Map[Food, Double] = {
    var map = Map[Food, Double]()
    var nameList = name.toUpperCase.trim
    for (item <- foodList.keys) {
      var ingredients = item.ingredients.keys.map(_.name.toUpperCase.trim).mkString(" ")
      if (ingredients.contains(nameList)) {
        map += (item -> foodList(item))
      }
    }
    map
  }

  def getByAvailability(num: Double): Map[Food, Double] = {
    foodList.filter(_._2 >= num)
  }

}