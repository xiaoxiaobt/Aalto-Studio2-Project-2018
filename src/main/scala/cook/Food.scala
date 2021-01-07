package cook
import collection.mutable._

case class Food(name: String, ingredients: Map[Food, Double],
  main_unit: String, second_unit: String, density: Double, tag: String, description: String) {
  
  private var menu_type: Boolean = false

  def setToMenu(): Unit = menu_type = true

  def setToRaw(): Unit = menu_type = false

  def isMenu: Boolean = menu_type

  def hasNoIngredients: Boolean = ingredients.isEmpty

}