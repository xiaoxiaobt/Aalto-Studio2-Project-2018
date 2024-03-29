package cook
import scala.swing.*
import scala.swing.Alignment.Left
import scala.swing.BorderPanel.Position.{West, East}
import scala.swing.Orientation.{Horizontal, Vertical}
import scala.swing.Swing.{
  Icon,
  HStrut,
  LineBorder,
  EmptyBorder,
  EmptyIcon,
  pair2Dimension
}
import java.awt.Color.{WHITE, GREEN, BLUE, RED, ORANGE}

class UISectionBox(food: Food, ui: UI) extends BoxPanel(Vertical) {

  private val menu: FoodMenu = ui.menu
  private val myColor: Color = Settings.color

  val firstRow = BoxPanel(Horizontal)
  val labelName = Label(" " + "%-28s".format(food.name), EmptyIcon, Left)
  val firstRowIconset = BoxPanel(Horizontal)
  val iconBoxes = Array.fill[Button](6)(Button("") {})

  def p[T](a: T) = if (Settings.diagnosis) println(a.toString)

  val buttonDelete: Button = Button(" x ") {
    menu.foodMap -= food
    p("Notice: %s has been removed from the list".format(food.name))
    ui.revalidateWindow(this)
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
    ui.leftMultifunctionalText.border = LineBorder(myColor, 5)
    ui.leftFeedback.text =
      "> Edit menu in given format (Example below), press green Complete button when finished"
  }

  val buttonModify: Button = Button("") {
    ui.rightCheckboxList.foreach(_.visible = false)
    ui.buttonSave.visible = false
    ui.rightBox.revalidate()
    ui.leftMultifunctionalButton.visible = true
    ui.deafTo(ui.searchBox)
    val editString =
      food.name + "\t" + food.getIngredientsString + "\t" + food.tag.mkString + "\t" + food.description + "\t" +
        food.isMenu + "\t" + menu.foodMap(food).toString
    p("Editing string: " + editString)
    ui.leftMultifunctionalText.text = editString
    ui.leftMultifunctionalText.editable = true
    ui.leftMultifunctionalText.border = LineBorder(myColor, 5)
    ui.leftFeedback.text =
      "> Edit menu in given format in the box below, press green Complete button when finished"
  }

  private val editIcon = Icon("src/main/scala/icons/edit.png")
  buttonModify.icon = editIcon
  val labelDescription = Label("   Description: " + food.description)
  def d2i(num: Double): String =
    if (num.toInt.toDouble == num) num.toInt.toString else num.toString
  val labelIngredient = Label(
    "   Ingredients: " + food.getIngredientsString
  )
  if (food.hasNoIngredients)
    labelIngredient.text = "   " + food.name + " is an ingredient. "
  val firstPart = BorderPanel()
  val secondPart = BorderPanel()
  val thirdPart = BorderPanel()
  val lastPart = BorderPanel()
  val lastRow = BoxPanel(Horizontal)
  val readyAmountInt = menu.foodMap(food).toInt
  val labelReady = Label("Ready to eat: " + readyAmountInt.toString)

  if (food.hasNoIngredients)
    labelReady.text = "Amount: " + readyAmountInt.toString
  val labelCookable = Label(
    "Cookable: " + (menu.checkAvailability(food) - readyAmountInt).toString
  )
  if (food.hasNoIngredients) labelCookable.visible = false
  val buttonMake = Button("  Use/Make  ") {
    menu.makeDish(food, 1)
    p("Notice: 1 portion of " + food.name + " has been made/consumed")
    if (!ui.changed) ui.refreshMenuBox() else ui.changeBox(ui.searchBox.text)
    ui.leftNormalMenuBox.contents -= this
    ui.outerBox.revalidate()
  }

  // First row
  labelName.font = Font("Consolas", Font.Plain, 48)
  labelName.foreground = myColor
  labelName.preferredSize = (1330, 60)
  for (x <- iconBoxes) {
    firstRowIconset.contents += x
    x.border = EmptyBorder
    x.background = WHITE
    x.preferredSize = (30, 30)
  }

  buttonAdd.font = Font("Arial", Font.Plain, 40)
  buttonAdd.border = EmptyBorder
  buttonAdd.opaque = false
  buttonAdd.background = WHITE
  buttonAdd.foreground = GREEN
  buttonModify.font = Font("Arial", Font.Plain, 40)
  buttonModify.border = EmptyBorder
  buttonModify.opaque = false
  buttonModify.background = WHITE
  buttonModify.foreground = BLUE
  buttonDelete.font = Font("Arial", Font.Plain, 40)
  buttonDelete.border = EmptyBorder
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
  labelDescription.font = Font("Arial", Font.Plain, 36)
  // Third row: Ingredients
  labelIngredient.font = Font("Arial", Font.Plain, 36)
  // Last row: Cooked, Cookable & Make
  labelReady.font = Font("Arial", Font.Plain, 30)
  labelCookable.font = Font("Arial", Font.Plain, 30)
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
  buttonMake.font = Font("Arial", Font.Plain, 32)
  buttonMake.background = WHITE
  buttonMake.border = LineBorder(myColor, 2)
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

  this.contents += firstPart
  this.contents += secondPart
  this.contents += thirdPart
  this.contents += lastPart
  this.preferredSize = (1330, 200)
  this.border = LineBorder(myColor, 1)
}
