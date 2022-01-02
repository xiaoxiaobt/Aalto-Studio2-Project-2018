package cook
import scala.swing._
import scala.swing.BorderPanel.Position.{West, East}
import scala.swing.Orientation.{Horizontal, Vertical}
import scala.swing.Alignment.Left
import java.awt.Color.{WHITE, GREEN, BLUE, RED, ORANGE}
import Swing.{Icon, HStrut}
import javax.swing.BorderFactory
import scala.collection.mutable.ArrayBuffer

class UISectionBox(food: Food, ui: UI) {

  private val menu: FoodMenu = ui.menu
  private val myColor: Color = Settings.color
  def p[T](a: T) = if (Settings.diagnosis) println(a.toString)
  val defaultBox = new BoxPanel(Vertical)
  val firstRow = new BoxPanel(Horizontal)
  val labelName = new Label(" " + food.name + " " * (28 - food.name.length))
  val firstRowIconset = new BoxPanel(Horizontal)
  val iconBoxes = ArrayBuffer.fill[Button](6)(Button("") {})

  val buttonDelete: Button = Button(" x ") {
    menu.foodMap -= food
    p("Notice: " + food.name + " has been removed from the list")
    ui.revalidateWindow(defaultBox)
  }
  val buttonAdd: Button = Button(" + ") {
    ui.rightCheckboxList.foreach(_.visible = false)
    ui.buttonSave.visible = false
    ui.leftMultifunctionalButton.visible = true
    ui.deafTo(ui.searchBox)
    if (!ui.changed) ui.refreshMenuBox() else ui.changeBox(ui.searchBox.text)
    ui.leftMenuScroll.revalidate()
    val editString =
      "Example_name\tIngredient_a=1,Ingredient_b=2\tTAG\tDescription\tisMenu (true/false)\tamount"
    p("Adding string: " + editString)
    ui.leftMultifunctionalText.editable = true
    ui.leftMultifunctionalText.text = editString
    ui.leftMultifunctionalText.border =
      BorderFactory.createLineBorder(myColor, 5)
    ui.leftFeedback.text =
      "> Edit menu in given format (Example below), press green Complete button when finished"
  }
  val buttonModify: Button = Button("") {
    ui.rightCheckboxList.foreach(_.visible = false)
    ui.buttonSave.visible = false
    ui.rightBox.revalidate()
    ui.leftMultifunctionalButton.visible = true
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
      food.name + "\t" + ingredientsString + "\t" + food.tag.mkString + "\t" + food.description + "\t" + food.isMenu + "\t" + menu
        .foodMap(food)
        .toString
    p("Editing string: " + editString)
    ui.leftMultifunctionalText.text = editString
    ui.leftMultifunctionalText.editable = true
    ui.leftMultifunctionalText.border =
      BorderFactory.createLineBorder(myColor, 5)
    ui.leftFeedback.text =
      "> Edit menu in given format in the box below, press green Complete button when finished"
  }
  private val editIcon = Icon("src/main/scala/icons/edit.png")
  buttonModify.icon = editIcon
  val labelDescription = new Label("   Description: " + food.description)
  def d2i(num: Double): String =
    if (num.toInt.toDouble == num) num.toInt.toString else num.toString
  val labelIngredient = new Label(
    "   Ingredients: " + food.ingredients.toList
      .map(x => x._1.name + "×" + d2i(x._2))
      .mkString(", ")
  )
  if (food.hasNoIngredients)
    labelIngredient.text = "   " + food.name + " is an ingredient. "
  val firstPart = new BorderPanel
  val secondPart = new BorderPanel
  val thirdPart = new BorderPanel
  val lastPart = new BorderPanel
  val lastRow = new BoxPanel(Horizontal)
  val labelReady = new Label(
    "Ready to eat: " + menu.foodMap(food).toInt.toString
  )
  if (food.hasNoIngredients)
    labelReady.text = "Amount: " + menu.foodMap(food).toInt.toString
  val labelCookable = new Label(
    "Cookable: " + (menu
      .checkAvailability(food) - menu.foodMap(food).toInt).toString
  )
  if (food.hasNoIngredients) labelCookable.visible = false
  val buttonMake =
    Button(if (food.hasNoIngredients) "       Use       " else "  Use/Make  ") {
      menu.makeDish(food, 1)
      p("Notice: 1 " + food.name + " has been made/consumed")
      if (!ui.changed) ui.refreshMenuBox() else ui.changeBox(ui.searchBox.text)
      ui.leftNormalMenuBox.contents -= defaultBox
      ui.outerBox.revalidate()
    }

  // First row
  labelName.font = new Font("Consolas", 0, 48)
  labelName.foreground = myColor
  labelName.horizontalAlignment = Left
  labelName.preferredSize = new Dimension(1330, 60)
  for (x <- iconBoxes) {
    firstRowIconset.contents += x
    x.border = BorderFactory.createEmptyBorder()
    x.background = WHITE
    x.preferredSize = new Dimension(30, 30)
  }

  buttonAdd.font = new Font("Arial", 0, 40)
  buttonAdd.border = BorderFactory.createEmptyBorder()
  buttonAdd.opaque = false
  buttonAdd.background = WHITE
  buttonAdd.foreground = GREEN
  buttonModify.font = new Font("Arial", 0, 40)
  buttonModify.border = BorderFactory.createEmptyBorder()
  buttonModify.opaque = false
  buttonModify.background = WHITE
  buttonModify.foreground = BLUE
  buttonDelete.font = new Font("Arial", 0, 40)
  buttonDelete.border = BorderFactory.createEmptyBorder()
  buttonDelete.opaque = false
  buttonDelete.background = WHITE
  buttonDelete.foreground = RED
  firstRow.contents += labelName
  firstRow.contents += firstRowIconset
  firstRow.contents += HStrut(280)
  firstRow.contents += buttonAdd
  firstRow.contents += buttonModify
  firstRow.contents += buttonDelete

  val tagPair: Set[(Char, Int)] = Settings.allAbbreviations.zipWithIndex
  // Icon A/G/L/M/V/W
  for ((letter, index) <- tagPair) {
    val c = if (food.tag.contains(letter)) "B_" else "W_"
    iconBoxes(index).icon = Icon("src/main/scala/icons/" + c + letter + ".png")
  }

  // Second row: Description
  labelDescription.font = new Font("Arial", 0, 36)
  // Third row: Ingredients
  labelIngredient.font = new Font("Arial", 0, 36)
  // Last row: Cooked, Cookable & Make
  labelReady.font = new Font("Arial", 0, 30)
  labelCookable.font = new Font("Arial", 0, 30)
  if (menu.foodMap(food) > 0)
    labelReady.foreground = ORANGE
  else
    labelReady.visible = false

  if (menu.checkAvailability(food) > 0) labelCookable.foreground = GREEN
  else {
    labelCookable.foreground = RED
    labelCookable.text = "Available: 0"
    buttonMake.enabled = false
  }
  buttonMake.font = new Font("Arial", 0, 32)
  buttonMake.background = WHITE
  buttonMake.border = BorderFactory.createLineBorder(myColor, 2)
  lastRow.contents += labelReady
  lastRow.contents += HStrut(20)
  lastRow.contents += labelCookable
  lastRow.contents += HStrut(20)
  lastRow.contents += buttonMake
  lastRow.contents += HStrut(10)

  firstPart.layout(firstRow) = West
  secondPart.layout(labelDescription) = West
  thirdPart.layout(labelIngredient) = West
  lastPart.layout(lastRow) = East

  defaultBox.contents += firstPart
  defaultBox.contents += secondPart
  defaultBox.contents += thirdPart
  defaultBox.contents += lastPart
  defaultBox.preferredSize = new Dimension(1330, 200)
  defaultBox.border = BorderFactory.createLineBorder(myColor, 1)
}
