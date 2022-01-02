package cook
import scala.collection.mutable.Map

case class Food(
    val name: String,
    val ingredients: Map[Food, Double],
    val tag: Set[Char],
    val description: String
) {

  private var menuType: Boolean = false

  def setToMenu(): Unit = menuType = true

  def setToRaw(): Unit = menuType = false

  def isMenu: Boolean = menuType

  def hasNoIngredients: Boolean = ingredients.isEmpty

}
