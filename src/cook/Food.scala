package cook
import collection.mutable._

class Food(val name: String, var ingredients: Map[Food, Double],
  val main_unit: String, val second_unit: String, val density: Double, var tag: String, var description: String) {
  private var menu_type: Boolean = false

  def set_to_menu(): Unit = menu_type = true

  def set_to_raw(): Unit = menu_type = false

  def is_menu(): Boolean = menu_type

}