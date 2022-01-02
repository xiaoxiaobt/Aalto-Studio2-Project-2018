package cook
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.Orientation._
import scala.swing.Alignment._
import scala.swing.event._
import javax.swing.BorderFactory
import Swing._
import scala.collection.mutable._

class UISearchRepresentation(ui: UI, keyword: String) {
  private val menu: FoodMenu = ui.menu
  private val myColor: Color = Settings.color
  private val key: Double = keyword.toDoubleOption.getOrElse(Double.NaN)

  def allergiesRemove(map: Map[Food, Double]): Map[Food, Double] = {
    //var allergies = (Settings.all_abbri zip ui.rightCheckboxList.map(_.selected)).filter(_._2).map(_._1)
    val allergies = ui.rightCheckboxList.filter(_.selected).map(_.name)
    map //.filter(x => allergies.forall(y => x._1.tag.contains(y)))
  }

  val headline = new Label(" Search Results: You have searched \"" + key + "\"")
  val headlineBorder = new BorderPanel

  // Headline
  headline.horizontalAlignment = Left
  headline.font = new Font("Arial", 0, 40)

  // Headline frame
  headlineBorder.layout(headline) = West

  def addSubFrame(labelName: String, result: Map[Food, Double]): BorderPanel = {
    val line = new Label("  >Search by Name")
    val lineBorder = new BorderPanel
    val boxBorder = new BorderPanel
    val box = new BoxPanel(Vertical)
    line.horizontalAlignment = Left
    line.font = new Font("Arial", 0, 40)
    line.foreground = myColor

    // Box
    lineBorder.layout(line) = West
    box.contents += lineBorder
    for ((itemFood, itemAmount) <- result)
      box.contents += new UISectionBox(itemFood, ui).defaultBox
    if (box.contents.size == 1) {
      val label = new Label("  No matches")
      label.font = new Font("Arial", 0, 36)
      val border = new BorderPanel
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
