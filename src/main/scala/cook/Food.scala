package cook
import scala.collection.parallel.mutable.ParHashMap

case class Food(
    val name: String,
    val ingredients: ParHashMap[Food, Double],
    val tag: Set[Char],
    val description: String
) {

  private var menuType: Boolean = false

  def setToMenu(): Unit = menuType = true

  def setToRaw(): Unit = menuType = false

  def isMenu: Boolean = menuType

  def hasNoIngredients: Boolean = ingredients.isEmpty

  def getIngredientsString: String = ingredients
    .map((f, amount) => f.name + "=" + amount.toString)
    .mkString(",")

}
