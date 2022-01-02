package cook
import scala.swing._
import scala.swing.BorderPanel.Position.West
import scala.swing.Orientation.Vertical
import scala.swing.Alignment.Left
import scala.swing.event._
import scala.collection.parallel.mutable.ParHashMap

class UISearchRepresentation(ui: UI, keyword: String) {
  private val menu: FoodMenu = ui.menu
  private val myColor: Color = Settings.color
  private val key: Double = keyword.toDoubleOption.getOrElse(Double.NaN)

  def allergiesRemove(
      map: ParHashMap[Food, Double]
  ): ParHashMap[Food, Double] = {
    // var allergies = (Settings.all_abbri zip ui.rightCheckboxList.map(_.selected)).filter(_._2).map(_._1)
    val allergies = ui.rightCheckboxList
      .zip(Settings.allAbbreviations)
      .filter(_._1.selected)
      .map(_._2)
    map.filter(x => allergies.forall(y => x._1.tag.contains(y)))
  }

  val headline = Label(" Search Results: You have searched \"" + key + "\"")
  val headlineBorder = BorderPanel()

  // Headline
  headline.horizontalAlignment = Left
  headline.font = Font("Arial", Font.Plain, 40)

  // Headline frame
  headlineBorder.layout(headline) = West

  def addSubFrame(
      labelName: String,
      result: ParHashMap[Food, Double]
  ): BorderPanel = {
    val line = Label("  >Search by Name")
    val lineBorder = BorderPanel()
    val boxBorder = BorderPanel()
    val box = BoxPanel(Vertical)
    line.horizontalAlignment = Left
    line.font = Font("Arial", Font.Plain, 40)
    line.foreground = myColor

    // Box
    lineBorder.layout(line) = West
    box.contents += lineBorder
    for ((itemFood, itemAmount) <- result)
      box.contents += UISectionBox(itemFood, ui).defaultBox
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

  val title1 = "  >Search by Name"
  val title2 = "  >Search by Ingredients"
  val title3 = "  >Search by Amount"
  val result1 = allergiesRemove(menu.getByName(keyword.trim))
  val result2 = allergiesRemove(menu.getByIngredients(keyword.trim))
  val result3 = allergiesRemove(menu.getByAvailability(key))
  val box1Border: BorderPanel = addSubFrame(title1, result1)
  val box2Border: BorderPanel = addSubFrame(title2, result2)
  val box3Border: BorderPanel = addSubFrame(title3, result3)
}
