package cook
import scala.swing.*
import scala.swing.event.*
import scala.swing.BorderPanel.Position.West
import scala.swing.Orientation.Vertical
import scala.swing.Alignment.Left
import scala.swing.Swing.EmptyIcon
import scala.collection.parallel.ParIterable
import scala.collection.parallel.mutable.ParHashMap


class UISearchRepresentation(ui: UI, keyword: String) {
  private val menu: FoodMenu = ui.menu
  private val myColor: Color = Settings.color
  private val key: Double = keyword.toDoubleOption.getOrElse(Double.NaN)

  val headline = Label(
    " Search Results: You have searched \"" + keyword + "\"",
    EmptyIcon,
    Left
  )
  val headlineBorder = BorderPanel()

  // Headline
  headline.font = Font("Arial", Font.Plain, 40)

  // Headline frame
  headlineBorder.layout(headline) = West

  private def allergiesRemove(map: ParHashMap[Food, Double]) = {
    val allergies = ui.returnStatus()
    map.keys.filter(x => allergies.forall(y => x.tag.contains(y)))
  }

  private def addSubFrame(
      labelName: String,
      result: ParIterable[Food]
  ): BorderPanel = {
    val line = Label("  >Search by Name", EmptyIcon, Left)
    val lineBorder = BorderPanel()
    val boxBorder = BorderPanel()
    val box = BoxPanel(Vertical)
    line.font = Font("Arial", Font.Plain, 40)
    line.foreground = myColor

    // Box
    lineBorder.layout(line) = West
    box.contents += lineBorder
    for (itemFood <- result)
      box.contents += UISectionBox(itemFood, ui)
    if (box.contents.size == 1) {
      val label = Label("  No matches")
      label.font = Font("Arial", Font.Plain, 36)
      val border = BorderPanel()
      border.layout(label) = West
      box.contents += border
    }

    // Add Frame
    boxBorder.layout(box) = West
    boxBorder
  }

  private val title1 = "  >Search by Name"
  private val title2 = "  >Search by Ingredients"
  private val title3 = "  >Search by Amount"
  private val result1 = allergiesRemove(menu.getByName(keyword.trim))
  private val result2 = allergiesRemove(menu.getByIngredients(keyword.trim))
  private val result3 = allergiesRemove(menu.getByAvailability(key))
  val box1Border: BorderPanel = addSubFrame(title1, result1)
  val box2Border: BorderPanel = addSubFrame(title2, result2)
  val box3Border: BorderPanel = addSubFrame(title3, result3)
}
