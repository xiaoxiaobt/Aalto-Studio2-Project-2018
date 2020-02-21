package cook
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.Orientation._
import scala.swing.Alignment._
import scala.swing.event._
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.io.Source._
import scala.util.control.Breaks._
import javax.swing.{ ImageIcon, BorderFactory }
import Swing._
import Settings.scaleTo
import scala.collection.mutable._

class UISearchRepresentation(ui: UI, keyword: String) {
  var menu = ui.menu
  var fridge = ui.menu.fridge
  var myColor = ui.settings.color
  var key = keyword.trim
  var keyDouble: Double = Double.NaN

  try {
    keyDouble = key.toDouble
  } catch {
    case e: NumberFormatException =>
  }

  def allergies_remove(map: Map[Food, Double]): Map[Food, Double] = {
    var allergies = (Settings.all_abbri zip ui.rightCheckboxList.map(_.selected)).filter(_._2).map(_._1)
    map.filter(x => allergies.forall(y => x._1.tag.contains(y)))
  }

  var headline = new Label(" Search Results: You have searched \"" + key + "\"")
  var headlineBorder = new BorderPanel
  var line1 = new Label("  >Search by Name")
  var line1Border = new BorderPanel
  var box1Border = new BorderPanel
  var box1 = new BoxPanel(Vertical)
  var line2 = new Label("  >Search by Ingredients")
  var line2Border = new BorderPanel
  var box2Border = new BorderPanel
  var box2 = new BoxPanel(Vertical)
  var line3 = new Label("  >Search by Amount")
  var line3Border = new BorderPanel
  var box3Border = new BorderPanel
  var box3 = new BoxPanel(Vertical)

  // Headline
  headline.horizontalAlignment = Left
  headline.font = new Font("Arial", 0, scaleTo(40))

  // Headline frame
  headlineBorder.layout(headline) = West

  // Line 1
  line1.horizontalAlignment = Left
  line1.font = new Font("Arial", 0, scaleTo(40))
  line1.foreground = myColor

  // Box 1
  var result1 = allergies_remove(fridge.getByName(key))
  line1Border.layout(line1) = West
  box1.contents += line1Border
  for ((item_food, item_amount) <- result1) box1.contents += new UISectionBox(item_food, ui).default_box
  if (box1.contents.size == 1) {
    var label_no_1 = new Label("  No matches")
    label_no_1.font = new Font("Arial", 0, scaleTo(36))
    var border_no_1 = new BorderPanel
    border_no_1.layout(label_no_1) = West
    box1.contents += border_no_1
  }

  // Frame 1 + Add
  box1Border.layout(box1) = West

  // Line 2
  line2.horizontalAlignment = Left
  line2.font = new Font("Arial", 0, scaleTo(40))
  line2.foreground = myColor

  // Box 2
  var result2 = allergies_remove(fridge.getByIngredients(key))
  line2Border.layout(line2) = West
  box2.contents += line2Border
  for ((item_food, item_amount) <- result2) box2.contents += new UISectionBox(item_food, ui).default_box
  if (box2.contents.size == 1) {
    var label_no_2 = new Label("  No matches")
    label_no_2.font = new Font("Arial", 0, scaleTo(36))
    var border_no_2 = new BorderPanel
    border_no_2.layout(label_no_2) = West
    box2.contents += border_no_2
  }

  // Frame 2
  box2Border.layout(box2) = West

  // Line 3
  line3.horizontalAlignment = Left
  line3.font = new Font("Arial", 0, scaleTo(40))
  line3.foreground = myColor

  // Box 3
  var result3 = allergies_remove(fridge.getByAvailability(keyDouble))
  line3Border.layout(line3) = West
  box3.contents += line3Border
  for ((item_food, item_amount) <- result3) box3.contents += new UISectionBox(item_food, ui).default_box

  if (box3.contents.size == 1) {
    var label_no_3 = new Label("  No matches")
    label_no_3.font = new Font("Arial", 0, scaleTo(36))
    var border_no_3 = new BorderPanel
    border_no_3.layout(label_no_3) = West
    box3.contents += border_no_3
  }

  // Frame 3
  box3Border.layout(box3) = West
}