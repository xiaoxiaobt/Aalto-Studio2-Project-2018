package cook
import collection.mutable._

class Fridge {
  var food_list = Map[Food, Double]()
  var settings = Settings
  override def toString() = {
    food_list.toString()
  }
  def food_list_raw = food_list.filter(_._1.ingredients.isEmpty)
  def food_list_cooked = food_list.filter(_._1.ingredients.nonEmpty)
  def p[T](a: T): Unit = if (settings.diagnosis) println(a.toString)

  def remove_food(food: Food, amount: Double): Boolean = {
    if (food_list.contains(food) && amount >= 0) {
      if (food_list(food) < amount) {
        false
      } else {
        food_list = food_list updated (food, food_list(food) - amount)
        true
      }
    } else {
      false
    }
  }
  def add_food(food: Food, amount: Double): Boolean = {
    if (amount > 0) {
      if (food_list.contains(food)) {
        food_list = food_list updated (food, food_list(food) + amount)
      } else {
        food_list = food_list + (food -> amount)
      }
      true
    } else {
      false
    }
  }
  def get_by_tags(tag: String): Map[Food, Double] = {
    var map = Map[Food, Double]()
    var tag_list = tag.toUpperCase().trim().split("").distinct
    if (tag.trim().isEmpty) {
      food_list
    } else {
      for (item <- food_list.keys) {
        var item_tag_list = item.tag.toUpperCase().trim().split("").distinct
        if (tag_list.intersect(item_tag_list).size == tag_list.size) {
          map = map + (item -> food_list(item))
        }
      }
      map
    }
  }
  def get_by_name(name: String): Map[Food, Double] = {
    var map = Map[Food, Double]()
    var name_list = name.toUpperCase().trim
    for (item <- food_list.keys) {
      var item_name_list = item.name.toUpperCase().trim
      if (item_name_list matches ".*" + name_list + ".*") {
        map = map + (item -> food_list(item))
      }
    }
    map

  }

  def get_by_ingredients(name: String): Map[Food, Double] = {
    var map = Map[Food, Double]()
    var name_list = name.toUpperCase().trim
    for (item <- food_list.keys) {
      var ingredients = item.ingredients.keys.map(_.name.toUpperCase.trim).mkString(" ")
      if (ingredients.contains(name_list)) {
        map = map + (item -> food_list(item))
      }
    }
    map
  }

  def get_by_availability(num: Double): Map[Food, Double] = {
    food_list.filter(_._2 >= num)
  }
}