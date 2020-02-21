package cook
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.Orientation._
import scala.swing.Alignment._
import scala.swing.event._
import java.awt.Color.{ BLACK, WHITE, GREEN, BLUE, RED, ORANGE, GRAY }
import Swing._
import java.io._
import javax.swing.{ ImageIcon, BorderFactory }
import scala.collection.mutable.ArrayBuffer
import Settings.scaleTo

class UISectionBox(food: Food, ui: UI) {
  var menu = ui.menu
  var my_color = Settings.color
  def p[T](a: T) = if (menu.settings.diagnosis) println(a.toString)
  var defaultBox = new BoxPanel(Vertical)
  var firstRow = new BoxPanel(Horizontal)
  var labelName = new Label(" " + food.name + " " * (28 - food.name.length))
  var firstRowIconset = new BoxPanel(Horizontal)
  var iconBoxes = ArrayBuffer.fill[Button](6)(Button("") {})
  var buttonDelete = Button(" x ") {
    menu.fridge.foodList -= food
    p("Notice: " + food.name + " has been removed from the list")
    ui.revalidateWindow(defaultBox)
  }
  var buttonAdd = Button(" + ") {
    ui.rightCheckboxList.foreach(_.visible = false)
    ui.buttonSave.visible = false
    ui.leftMultifunctionalButton.visible = true
    ui.edit = false
    ui.deafTo(ui.searchBox)
    if (!ui.changed) ui.refreshMenuBox() else ui.changeBox(ui.searchBox.text)
    ui.leftMenuScroll.revalidate()
    var edit_string = "Example_name:Ingredient_a=1,Ingredient_b=2:unit_1:unit_2:0:Allengens:Description:isMenudigit (1=yes, 0=no):amount"
    p("Adding string: " + edit_string)
    ui.leftMultifunctionalText.editable = true
    ui.leftMultifunctionalText.text = edit_string
    ui.leftMultifunctionalText.border = BorderFactory.createLineBorder(my_color, scaleTo(5))
    ui.leftFeedback.text = "> Edit menu in given format (Example below), press green Complete button when finished"
  }
  var buttonModify = Button("") {
    ui.rightCheckboxList.foreach(_.visible = false)
    ui.buttonSave.visible = false
    ui.rightBox.revalidate()
    ui.leftMultifunctionalButton.visible = true
    ui.edit = true
    ui.editing = food
    ui.deafTo(ui.searchBox)
    var boo = if (food.isMenu) "1" else "0"
    var ingredients_string = {
      if (food.ingredients.isEmpty) {
        ""
      } else {
        food.ingredients.toList.map(x => x._1.name + "=" + x._2.toString).mkString(",")
      }
    }
    var edit_string = food.name + ":" + ingredients_string + ":" + food.main_unit + ":" + food.second_unit + ":" + food.density.toString + ":" + food.tag + ":" + food.description + ":" + boo + ":" + menu.fridge.foodList(food).toString
    p("Editing string: " + edit_string)
    ui.leftMultifunctionalText.text = edit_string
    ui.leftMultifunctionalText.editable = true
    ui.leftMultifunctionalText.border = BorderFactory.createLineBorder(my_color, scaleTo(5))
    ui.leftFeedback.text = "> Edit menu in given format in the box below, press green Complete button when finished"
  }
  var editIcon = new ImageIcon("src/icons/edit.png")
  buttonModify.icon = editIcon
  var label_des = new Label("   Description: " + food.description)
  def d2i(num: Double) = if (num.toInt.toDouble == num) num.toInt.toString else num.toString
  var label_ingre = new Label("   Ingredients: " + food.ingredients.toList.map(x => x._1.name + "×" + d2i(x._2) + x._1.main_unit).mkString(", "))
  if (food.ingredients.isEmpty) label_ingre.text = "   " + food.name + " is an ingredient. "
  var first_part = new BorderPanel
  var second_part = new BorderPanel
  var third_part = new BorderPanel
  var last_part = new BorderPanel
  var last_row = new BoxPanel(Horizontal)
  var label_ready = new Label("Ready to eat: " + menu.fridge.foodList(food).toInt.toString)
  if (food.ingredients.isEmpty) label_ready.text = "Amount: " + menu.fridge.foodList(food).toInt.toString
  var label_cookable = new Label("Cookable: " + (menu.checkAvailability(food) - menu.fridge.foodList(food).toInt).toString)
  if (food.ingredients.isEmpty) label_cookable.visible = false
  var button_make = Button("  Use/Make  ") {
    menu.makeDish(food, 1)
    p("Notice: 1 " + food.name + " has been made/consumed")
    if (!ui.changed) ui.refreshMenuBox() else ui.changeBox(ui.searchBox.text)
    ui.leftNormalMenuBox.contents -= defaultBox
    ui.outerBox.revalidate()
  }
  if (food.ingredients.isEmpty) button_make.text = "       Use       "

  //first_row
  labelName.font = new Font("Consolas", 0, scaleTo(48))
  labelName.foreground = my_color
  labelName.horizontalAlignment = Left
  labelName.preferredSize = new Dimension(scaleTo(1330), scaleTo(60))
  for (x <- iconBoxes) {
    firstRowIconset.contents += x
    x.border = BorderFactory.createEmptyBorder()
    x.background = WHITE
    x.preferredSize = new Dimension(scaleTo(30), scaleTo(30))
  }
  buttonAdd.font = new Font("Arial", 0, scaleTo(40))
  buttonAdd.border = BorderFactory.createEmptyBorder()
  buttonAdd.opaque = false
  buttonAdd.background = WHITE
  buttonAdd.foreground = GREEN
  buttonModify.font = new Font("Arial", 0, scaleTo(40))
  buttonModify.border = BorderFactory.createEmptyBorder()
  buttonModify.opaque = false
  buttonModify.background = WHITE
  buttonModify.foreground = BLUE
  buttonDelete.font = new Font("Arial", 0, scaleTo(40))
  buttonDelete.border = BorderFactory.createEmptyBorder()
  buttonDelete.opaque = false
  buttonDelete.background = WHITE
  buttonDelete.foreground = RED
  firstRow.contents += labelName
  firstRow.contents += firstRowIconset
  firstRow.contents += HStrut(scaleTo(280))
  firstRow.contents += buttonAdd
  firstRow.contents += buttonModify
  firstRow.contents += buttonDelete

  var foodTag = food.tag.toUpperCase
  var tagPair = "AGLMVW".zipWithIndex
  // Icon A/G/L/M/V/W
  for ((letter, index) <- tagPair) {
    if (foodTag.contains(letter)) {
      iconBoxes(index).icon = new ImageIcon("src/icons/B_" + letter + ".png")
    } else {
      iconBoxes(index).icon = new ImageIcon("src/icons/W_" + letter + ".png")
    }
  }

  // Second row: Description
  label_des.font = new Font("Arial", 0, scaleTo(36))
  // Third row: Ingredients
  label_ingre.font = new Font("Arial", 0, scaleTo(36))
  // Last row: Cooked, Cookable & Make
  label_ready.font = new Font("Arial", 0, scaleTo(30))
  label_cookable.font = new Font("Arial", 0, scaleTo(30))
  if (menu.fridge.foodList(food) > 0) {
    label_ready.foreground = ORANGE
  } else {
    label_ready.visible = false
  }
  if (menu.checkAvailability(food) > 0) {
    label_cookable.foreground = GREEN
  } else {
    label_cookable.foreground = RED
    label_cookable.text = "Available: 0"
    button_make.enabled = false
  }
  button_make.font = new Font("Arial", 0, scaleTo(32))
  button_make.background = WHITE
  button_make.border = BorderFactory.createLineBorder(my_color, scaleTo(2))
  last_row.contents += label_ready
  last_row.contents += HStrut(scaleTo(20))
  last_row.contents += label_cookable
  last_row.contents += HStrut(scaleTo(20))
  last_row.contents += button_make
  last_row.contents += HStrut(scaleTo(10))

  first_part.layout(firstRow) = West
  second_part.layout(label_des) = West
  third_part.layout(label_ingre) = West
  last_part.layout(last_row) = East

  defaultBox.contents += first_part
  defaultBox.contents += second_part
  defaultBox.contents += third_part
  defaultBox.contents += last_part
  defaultBox.preferredSize = new Dimension(scaleTo(1330), scaleTo(200))
  defaultBox.border = BorderFactory.createLineBorder(my_color, scaleTo(1))

}