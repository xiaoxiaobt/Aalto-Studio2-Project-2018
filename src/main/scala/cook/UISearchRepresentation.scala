package cook
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.Orientation._
import scala.swing.Alignment._
import scala.swing.event._
import scala.io.Source._
import javax.swing.BorderFactory
import Swing._
import Settings.scaleTo
import scala.collection.mutable._

class UISearchRepresentation(ui: UI, keyword: String) {
  var menu = ui.menu
  var myColor = Settings.color
  var key = keyword.trim
  var keyDouble: Double = Double.NaN

  try {
    keyDouble = key.toDouble
  } catch {
    case e: NumberFormatException =>
  }

  def allergiesRemove(map: Map[Food, Double]): Map[Food, Double] = {
    //var allergies = (Settings.all_abbri zip ui.rightCheckboxList.map(_.selected)).filter(_._2).map(_._1)
    var allergies = ui.rightCheckboxList.filter(_.selected).map(_.name)
    map.filter(x => allergies.forall(y => x._1.tag.contains(y)))
  }

  var headline = new Label(" Search Results: You have searched \"" + key + "\"")
  var headlineBorder = new BorderPanel

  // Headline
  headline.horizontalAlignment = Left
  headline.font = new Font("Arial", 0, scaleTo(40))

  // Headline frame
  headlineBorder.layout(headline) = West

  def addSubFrame(labelName: String, result: Map[Food, Double]): BorderPanel = {
    var line = new Label("  >Search by Name")
    var lineBorder = new BorderPanel
    var boxBorder = new BorderPanel
    var box = new BoxPanel(Vertical)
    line.horizontalAlignment = Left
    line.font = new Font("Arial", 0, scaleTo(40))
    line.foreground = myColor

    // Box
    lineBorder.layout(line) = West
    box.contents += lineBorder
    for ((item_food, item_amount) <- result) box.contents += new UISectionBox(item_food, ui).defaultBox
    if (box.contents.size == 1) {
      var label = new Label("  No matches")
      label.font = new Font("Arial", 0, scaleTo(36))
      var border = new BorderPanel
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
  var result1 = allergiesRemove(menu.getByName(key))
  var result2 = allergiesRemove(menu.getByIngredients(key))
  var result3 = allergiesRemove(menu.getByAvailability(keyDouble))
  var box1Border = addSubFrame(title1, result1)
  var box2Border = addSubFrame(title2, result2)
  var box3Border = addSubFrame(title3, result3)
}