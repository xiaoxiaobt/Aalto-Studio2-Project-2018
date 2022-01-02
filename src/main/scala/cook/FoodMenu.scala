package cook
import collection.mutable.Map
import scala.collection.parallel.mutable.ParHashMap
import scala.collection.parallel.CollectionsHaveToParArray

class FoodMenu {

  var foodMap = ParHashMap[Food, Double]()

  def p[T](a: T) = if (Settings.diagnosis) println(a.toString)

  def getFoodArray = foodMap.keys.toParArray

  def addMenu(food: Food): Boolean = {
    if (foodMap.contains(food)) {
      food.setToMenu()
      true
    } else false
  }

  def deleteMenu(food: Food): Boolean = {
    if (foodMap.contains(food)) {
      food.setToRaw()
      true
    } else false
  }

  /** Returns the amount of food as an integer */
  def checkAvailability(food: Food): Int = {
    val testMap = foodMap.clone()
    var testState = true
    var i = 0
    if (foodMap.contains(food))
      while (testState) {
        testState = checkAmount(food, 1, testMap)
        if (testState) i += 1
      }
    i
  }

  def checkAmount(
      food: Food,
      num: Double,
      testMap: ParHashMap[Food, Double]
  ): Boolean = {
    if (foodMap.contains(food)) {
      val currentAmount = testMap(food)
      if (currentAmount >= num)
        testMap += (food -> (testMap(food) - num))
        true
      else if (currentAmount > 0) {
        testMap += (food -> 0)
        checkAmount(food, num - currentAmount, testMap)
      } else {
        if (food.ingredients.isEmpty) false
        else {
          for ((name, amount) <- food.ingredients) {
            if (!checkAmount(name, amount, testMap))
              return false
          }
          return true
        }
      }
    } else false
  }

  def returnFoodWithName(name: String): Option[Food] = {
    val result = getFoodArray.find(_.name == name)
    if (result.isDefined) Some(result.get) else None
  }

  def allIngredientsExist(names: Iterable[String]): Boolean = {
    names.map(returnFoodWithName).forall(_.isDefined)
  }

  def makeDish(food: Food, num: Double): Unit = {
    if (foodMap(food) >= num) foodMap(food) -= num
    else if (foodMap(food) > 0) {
      val temp = num - foodMap(food)
      foodMap(food) = 0
      makeDish(food, temp)
    } else food.ingredients.foreach(x => makeDish(x._1, x._2 * num))
  }

  def removeFood(food: Food, amount: Double): Boolean = {
    if (foodMap.contains(food) && amount >= 0 && foodMap(food) >= amount) {
      foodMap += (food -> (foodMap(food) - amount))
      true
    } else false
  }

  def addFood(food: Food, amount: Double): Boolean = {
    if (amount > 0) {
      if (foodMap.contains(food))
        foodMap += (food -> (foodMap(food) + amount))
      else
        foodMap += (food -> amount)
      true
    } else false
  }

  def getByTags(tag: String): ParHashMap[Food, Double] = {
    val tagList = tag.toUpperCase.trim.split("").distinct
    if (tag.trim.isEmpty) foodMap
    else {
      var map = ParHashMap[Food, Double]()
      for ((item, amount) <- foodMap) {
        val uniqueTags = item.tag
        // if (tagList.intersect(uniqueTags).length == tagList.length)
        map += (item -> amount)
      }
      map
    }
  }

  def getByName(name: String): ParHashMap[Food, Double] = {
    var map = ParHashMap[Food, Double]()
    val nameList = ".*" + name.toUpperCase.trim + ".*"
    for ((item, amount) <- foodMap) {
      val itemNameList = item.name.toUpperCase.trim
      if (itemNameList matches nameList)
        map += (item -> amount)
    }
    map
  }

  def getByIngredients(name: String): ParHashMap[Food, Double] = {
    var map = ParHashMap[Food, Double]()
    val nameList = name.toUpperCase.trim
    for ((item, amount) <- foodMap) {
      val ingredients =
        item.ingredients.keys.map(_.name.toUpperCase.trim).mkString(" ")
      if (ingredients.contains(nameList)) map += (item -> amount)
    }
    map
  }

  def getByAvailability(num: Double): ParHashMap[Food, Double] = {
    foodMap.filter(_._2 >= num)
  }

}
