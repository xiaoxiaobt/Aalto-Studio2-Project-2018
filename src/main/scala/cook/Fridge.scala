package cook
import collection.mutable.Map

object Fridge {
  var foodList = Map[Food, Double]()
  override def toString() = foodList.toString()
  def foodListRaw = foodList.filter(_._1.ingredients.isEmpty)
  def foodListCooked = foodList.filter(_._1.ingredients.nonEmpty)
  def p[T](a: T): Unit = if (Settings.diagnosis) println(a.toString)

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