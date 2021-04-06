package cook
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.Orientation._
import scala.swing.Alignment._
import java.awt.Color.{WHITE, GREEN, BLUE, RED, ORANGE}
import Swing._
import javax.swing.BorderFactory
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import Settings.scaleTo

class UISectionBox(food: Food, ui: UI) {
  implicit def bool2Str(bool: Boolean): String = if (bool) "1" else "0"

  var menu: FoodMenu = ui.menu
  var myColor: Color = Settings.color
  def p[T](a: T) = if (Settings.diagnosis) println(a.toString)
  var defaultBox = new BoxPanel(Vertical)
  var firstRow = new BoxPanel(Horizontal)
  var labelName = new Label(" " + food.name + " " * (28 - food.name.length))
  var firstRowIconset = new BoxPanel(Horizontal)
  var iconBoxes = ArrayBuffer.fill[Button](6)(Button("") {})
  var buttonDelete: Button = Button(" x ") {
    menu.foodList -= food
    p("Notice: " + food.name + " has been removed from the list")
    ui.revalidateWindow(defaultBox)
  }
  var buttonAdd: Button = Button(" + ") {
    ui.rightCheckboxList.foreach(_.visible = false)
    ui.buttonSave.visible = false
    ui.leftMultifunctionalButton.visible = true
    ui.edit = false
    ui.deafTo(ui.searchBox)
    if (!ui.changed) ui.refreshMenuBox() else ui.changeBox(ui.searchBox.text)
    ui.leftMenuScroll.revalidate()
    val edit_string =
      "Example_name:Ingredient_a=1,Ingredient_b=2:unit_1:unit_2:0:Allengens:Description:isMenudigit (1=yes, 0=no):amount"
    p("Adding string: " + edit_string)
    ui.leftMultifunctionalText.editable = true
    ui.leftMultifunctionalText.text = edit_string
    ui.leftMultifunctionalText.border =
      BorderFactory.createLineBorder(myColor, scaleTo(5))
    ui.leftFeedback.text =
      "> Edit menu in given format (Example below), press green Complete button when finished"
  }
  var buttonModify: Button = Button("") {
    ui.rightCheckboxList.foreach(_.visible = false)
    ui.buttonSave.visible = false
    ui.rightBox.revalidate()
    ui.leftMultifunctionalButton.visible = true
    ui.edit = true
    ui.editing = food
    ui.deafTo(ui.searchBox)
    val ingredientsString = {
      if (food.hasNoIngredients) ""
      else {
        food.ingredients.toList
          .map(x => x._1.name + "=" + x._2.toString)
          .mkString(",")
      }
    }
    val editString =
      food.name + ":" + ingredientsString + ":" + food.main_unit + ":" + food.second_unit + ":" + food.density.toString + ":" + food.tag + ":" + food.description + ":" + food.isMenu + ":" + menu
        .foodList(food)
        .toString
    p("Editing string: " + editString)
    ui.leftMultifunctionalText.text = editString
    ui.leftMultifunctionalText.editable = true
    ui.leftMultifunctionalText.border =
      BorderFactory.createLineBorder(myColor, scaleTo(5))
    ui.leftFeedback.text =
      "> Edit menu in given format in the box below, press green Complete button when finished"
  }
  var editIcon = Icon("src/main/scala/icons/edit.png")
  buttonModify.icon = editIcon
  var labelDescription = new Label("   Description: " + food.description)
  def d2i(num: Double) =
    if (num.toInt.toDouble == num) num.toInt.toString else num.toString
  var labelIngredient = new Label(
    "   Ingredients: " + food.ingredients.toList
      .map(x => x._1.name + "×" + d2i(x._2) + x._1.main_unit)
      .mkString(", ")
  )
  if (food.hasNoIngredients)
    labelIngredient.text = "   " + food.name + " is an ingredient. "
  var first_part = new BorderPanel
  var second_part = new BorderPanel
  var third_part = new BorderPanel
  var last_part = new BorderPanel
  var lastRow = new BoxPanel(Horizontal)
  var labelReady = new Label(
    "Ready to eat: " + menu.foodList(food).toInt.toString
  )
  if (food.hasNoIngredients)
    labelReady.text = "Amount: " + menu.foodList(food).toInt.toString
  var labelCookable = new Label(
    "Cookable: " + (menu
      .checkAvailability(food) - menu.foodList(food).toInt).toString
  )
  if (food.hasNoIngredients) labelCookable.visible = false
  var buttonMake = Button("  Use/Make  ") {
    menu.makeDish(food, 1)
    p("Notice: 1 " + food.name + " has been made/consumed")
    if (!ui.changed) ui.refreshMenuBox() else ui.changeBox(ui.searchBox.text)
    ui.leftNormalMenuBox.contents -= defaultBox
    ui.outerBox.revalidate()
  }
  if (food.hasNoIngredients) buttonMake.text = "       Use       "

  // First row
  labelName.font = new Font("Consolas", 0, scaleTo(48))
  labelName.foreground = myColor
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

  var foodTag: String = food.tag.toUpperCase
  var tagPair: Seq[(Char, Int)] = "AGLMVW".zipWithIndex
  // Icon A/G/L/M/V/W
  for ((letter, index) <- tagPair) {
    if (foodTag.contains(letter))
      iconBoxes(index).icon = Icon("src/main/scala/icons/B_" + letter + ".png")
    else
      iconBoxes(index).icon = Icon("src/main/scala/icons/W_" + letter + ".png")
  }

  // Second row: Description
  labelDescription.font = new Font("Arial", 0, scaleTo(36))
  // Third row: Ingredients
  labelIngredient.font = new Font("Arial", 0, scaleTo(36))
  // Last row: Cooked, Cookable & Make
  labelReady.font = new Font("Arial", 0, scaleTo(30))
  labelCookable.font = new Font("Arial", 0, scaleTo(30))
  if (menu.foodList(food) > 0)
    labelReady.foreground = ORANGE
  else
    labelReady.visible = false

  if (menu.checkAvailability(food) > 0)
    labelCookable.foreground = GREEN
  else {
    labelCookable.foreground = RED
    labelCookable.text = "Available: 0"
    buttonMake.enabled = false
  }
  buttonMake.font = new Font("Arial", 0, scaleTo(32))
  buttonMake.background = WHITE
  buttonMake.border = BorderFactory.createLineBorder(myColor, scaleTo(2))
  lastRow.contents += labelReady
  lastRow.contents += HStrut(scaleTo(20))
  lastRow.contents += labelCookable
  lastRow.contents += HStrut(scaleTo(20))
  lastRow.contents += buttonMake
  lastRow.contents += HStrut(scaleTo(10))

  first_part.layout(firstRow) = West
  second_part.layout(labelDescription) = West
  third_part.layout(labelIngredient) = West
  last_part.layout(lastRow) = East

  defaultBox.contents += first_part
  defaultBox.contents += second_part
  defaultBox.contents += third_part
  defaultBox.contents += last_part
  defaultBox.preferredSize = new Dimension(scaleTo(1330), scaleTo(200))
  defaultBox.border = BorderFactory.createLineBorder(myColor, scaleTo(1))
}
