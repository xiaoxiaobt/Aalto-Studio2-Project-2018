package cook
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.Orientation._
import scala.swing.Alignment._
import scala.swing.event._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source._
import scala.util.control.Breaks._
import java.awt.Color._
import java.io._
import javax.swing.ImageIcon
import javax.swing.BorderFactory
import Swing._

class UISearchRepresentation(ui: UI, keyword: String) {
  var menu = ui.menu
  var fridge = ui.menu.fridge
  var my_color = ui.settings.color
  var key = keyword.trim
  var keyDouble: Double = Double.NaN

  try {
    keyDouble = key.toDouble
  } catch {
    case e: NumberFormatException =>
  }

  def allergies_remove(map: scala.collection.mutable.Map[Food, Double]): scala.collection.mutable.Map[Food, Double] = {
    var allergies = (fridge.settings.all_abbri zip ui.right_checkbox_list.map(_.selected)).filter(_._2).map(_._1)
    map.filter(x => allergies.forall(y => x._1.tag.contains(y)))
  }

  var headline = new Label(" Search Results: You have searched \"" + key + "\"")
  var headline_border = new BorderPanel
  var line1 = new Label("  >Search by Name")
  var line1_border = new BorderPanel
  var box1_border = new BorderPanel
  var box1 = new BoxPanel(Vertical)
  var line2 = new Label("  >Search by Ingredients")
  var line2_border = new BorderPanel
  var box2_border = new BorderPanel
  var box2 = new BoxPanel(Vertical)
  var line3 = new Label("  >Search by Amount")
  var line3_border = new BorderPanel
  var box3_border = new BorderPanel
  var box3 = new BoxPanel(Vertical)

  // Headline
  headline.horizontalAlignment = Left
  headline.font = new Font("Arial", 0, 40)

  // Headline frame
  headline_border.layout(headline) = West

  // Line 1
  line1.horizontalAlignment = Left
  line1.font = new Font("Arial", 0, 40)
  line1.foreground = my_color

  // Box 1
  var result1 = allergies_remove(fridge.get_by_name(key))
  line1_border.layout(line1) = West
  box1.contents += line1_border
  for ((item_food, item_amount) <- result1) box1.contents += new UISectionBox(item_food, ui).default_box
  if (box1.contents.size == 1) {
    var label_no_1 = new Label("  No matches")
    label_no_1.font = new Font("Arial", 0, 36)
    var border_no_1 = new BorderPanel
    border_no_1.layout(label_no_1) = West
    box1.contents += border_no_1
  }

  // Frame 1 + Add
  box1_border.layout(box1) = West

  // Line 2
  line2.horizontalAlignment = Left
  line2.font = new Font("Arial", 0, 40)
  line2.foreground = my_color

  // Box 2
  var result2 = allergies_remove(fridge.get_by_ingredients(key))
  line2_border.layout(line2) = West
  box2.contents += line2_border
  for ((item_food, item_amount) <- result2) box2.contents += new UISectionBox(item_food, ui).default_box
  if (box2.contents.size == 1) {
    var label_no_2 = new Label("  No matches")
    label_no_2.font = new Font("Arial", 0, 36)
    var border_no_2 = new BorderPanel
    border_no_2.layout(label_no_2) = West
    box2.contents += border_no_2
  }

  // Frame 2
  box2_border.layout(box2) = West

  // Line 3
  line3.horizontalAlignment = Left
  line3.font = new Font("Arial", 0, 40)
  line3.foreground = my_color

  // Box 3
  var result3 = allergies_remove(fridge.get_by_availability(keyDouble))
  line3_border.layout(line3) = West
  box3.contents += line3_border
  for ((item_food, item_amount) <- result3) box3.contents += new UISectionBox(item_food, ui).default_box

  if (box3.contents.size == 1) {
    var label_no_3 = new Label("  No matches")
    label_no_3.font = new Font("Arial", 0, 36)
    var border_no_3 = new BorderPanel
    border_no_3.layout(label_no_3) = West
    box3.contents += border_no_3
  }

  // Frame 3
  box3_border.layout(box3) = West
}