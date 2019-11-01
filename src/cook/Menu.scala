package cook

import collection.mutable._

class Menu {

  var fridge = new Fridge()
  var settings = new Settings
  var menu = get_menu()
  var test_list = fridge.food_list
  var test_state = true

  def p[T](a: T) = if (settings.diagnosis) println(a.toString)

  def menu_foodlist = fridge.food_list.filter(_._1.is_menu())

  def non_menu_foodlist = fridge.food_list.filter(!_._1.is_menu())

  def get_menu() = fridge.food_list.filter(_._1.is_menu()).map(_._1).toVector

  def add_menu(food: Food): Boolean = {
    if (fridge.food_list.contains(food)) {
      food.set_to_menu()
      true
    } else {
      false
    }
  }

  def del_menu(food: Food): Boolean = {
    if (fridge.food_list.contains(food)) {
      food.set_to_raw()
      true
    } else {
      false
    }
  }

  def exisiting_amount(food: Food): Double = fridge.food_list(food)

  //def initialize(): Unit = {
  //  test_list = fridge.food_list ++ Map()
  //  test_state = true
  //}

  /**Returns the amount of food as an integer*/
  def check_availability(food: Food): Int = {
    test_list = fridge.food_list ++ Map()
    test_state = true
    if (fridge.food_list.map(_._1).toList.contains(food)) {
      var i = 0
      while (test_state) {
        check_amount(food, 1)
        if (test_state) i += 1
      }
      i.toInt
    } else {
      0
    }
  }

  def check_amount(food: Food, num: Double): Unit = {
    if (fridge.food_list.map(_._1).toList.contains(food)) {
      var current_amount = test_list(food)
      if (current_amount >= num) {
        test_list = test_list updated (food, test_list(food) - num)
      } else if (current_amount > 0) {
        test_list = test_list updated (food, 0)
        check_amount(food, num - current_amount)
      } else {
        if (food.ingredients.isEmpty) {
          test_state = false
        } else {
          food.ingredients.foreach(x => check_amount(x._1, x._2))
        }
      }
    } else {
      test_state = false
    }
  }

  def return_food_with_name(name: String): Option[Food] = {
    if (fridge.food_list.filter(_._1.name == name).size != 0) {
      Some(fridge.food_list.filter(_._1.name == name).head._1)
    } else {
      None
    }
  }

  def make(food: Food, num: Double): Unit = {
    if (fridge.food_list(food) >= num) {
      fridge.food_list(food) -= num
    } else if (fridge.food_list(food) > 0) {
      var temp = num - fridge.food_list(food)
      fridge.food_list(food) = 0
      make(food, temp)
    } else {
      food.ingredients.foreach(x => make(x._1, x._2 * num))
    }
  }

}