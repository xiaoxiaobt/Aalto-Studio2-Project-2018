package cook
import scala.collection.parallel.mutable.{ParArray, ParHashMap}
import scala.collection.parallel.CollectionsHaveToParArray
import scala.collection.mutable.{ArrayBuffer, Map}

class FoodMenu {

  val foodMap = ParHashMap[Food, Double]()

  def getFoodArray = foodMap.keys.toParArray

  /** Returns the amount of foods as an integer */
  def checkAvailability(food: Food): Int = {
    val testMap = foodMap.clone()
    var testState = true
    var counter = 0
    if (foodMap.contains(food))
      while (testState) {
        testState = checkAmount(food, 1, testMap)
        if (testState) counter += 1
      }
    counter
  }

  private def checkAmount(
      food: Food,
      num: Double,
      testMap: ParHashMap[Food, Double]
  ): Boolean = {
    if (foodMap.contains(food)) {
      val currentAmount = testMap(food)
      if (currentAmount >= num) {
        testMap(food) -= num
        true
      } else if (currentAmount > 0) {
        testMap(food) = 0
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
    } else {
      food.ingredients.foreach((f, amount) => makeDish(f, amount * num))
    }
  }

  def addFood(food: Food, amount: Double): Boolean = {
    if (amount > 0) {
      if (foodMap.contains(food))
        foodMap(food) += amount
      else
        foodMap(food) = amount
      true
    } else false
  }

  // Only used in unit tests
  def getByTags(tag: String): ParArray[Food] = {
    val tagSet = tag.toUpperCase.toSet.intersect(
      Settings.allAbbreviations
    )
    if (tagSet.isEmpty) foodMap.keys.toParArray
    else {
      val foods = ArrayBuffer[Food]()
      for ((item, amount) <- foodMap) {
        val uniqueTags = item.tag
        if (tagSet.intersect(item.tag).size == tagSet.size)
          foods += item
      }
      foods.toParArray
    }
  }

  def getByName(key: String): ParArray[Food] = {
    val foods = ArrayBuffer[Food]()
    val name = key.trim.toUpperCase
    for ((item, amount) <- foodMap) {
      val itemName = item.name.trim.toUpperCase
      if (itemName.contains(name)) foods += item
    }
    foods.toParArray
  }

  def getByIngredients(name: String): ParArray[Food] = {
    val foods = ArrayBuffer[Food]()
    val ingredientName = name.trim.toUpperCase
    for ((item, amount) <- foodMap) {
      val ingredients =
        item.ingredients.keys.map(_.name.trim.toUpperCase).mkString(" ")
      if (ingredients.contains(ingredientName)) foods += item
    }
    foods.toParArray
  }

  def getByAvailability(num: Double): ParArray[Food] = {
    foodMap.filter(_._2 >= num).keys.toParArray
  }

}
