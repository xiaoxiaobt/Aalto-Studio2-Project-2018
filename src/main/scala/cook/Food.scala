package cook
import scala.collection.parallel.mutable.ParHashMap

case class Food(
    name: String,
    ingredients: ParHashMap[Food, Double],
    tag: Set[Char],
    description: String
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
