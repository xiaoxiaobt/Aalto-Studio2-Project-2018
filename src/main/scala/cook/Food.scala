package cook
import collection.mutable._

case class Food(val name: String, var ingredients: Map[Food, Double],
  val main_unit: String, val second_unit: String, val density: Double, var tag: String, var description: String) {
  private var menu_type: Boolean = false

  def setToMenu(): Unit = menu_type = true

  def setToRaw(): Unit = menu_type = false

  def isMenu: Boolean = menu_type

  def hasNoIngredients = ingredients.isEmpty

}