package cook
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.Orientation._
import scala.swing.Alignment._
import scala.swing.event._
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.io.Source._
import java.awt.Color.{BLACK, GRAY, RED, WHITE}
import javax.swing.BorderFactory.{createEmptyBorder, createLineBorder}
import javax.swing.ImageIcon
import Swing._
import Settings.scaleTo

class UI extends MainFrame {
  title = "Smart Cookbook"
  preferredSize = new Dimension(scaleTo(1920), scaleTo(1080))

  // Initialize
  var menu: FoodMenu = new FoodMenu()
  var settings = Settings
  def foodList = menu.foodList
  var myColor = settings.color
  var changed = false
  var edit = false
  var editing: Food = null
  private var tempSearchText = ""
  var fileProcessor = new FileProcessor(this)

  // Frames, boxes and buttons (Almost all boxes)
  var outerBox = new BoxPanel(Horizontal)
  var leftBox = new BorderPanel
  var leftInfoSection = new BoxPanel(Vertical)
  var leftWelcome = new Label("What would you like to eat today? ")
  leftWelcome.horizontalAlignment = Left
  var leftMenuScroll = new ScrollPane()
  var leftNormalMenuBox = new BoxPanel(Vertical)
  var leftSearchArea = new BoxPanel(Horizontal)
  var searchPreventionBox = new TextField("")
  var searchBox = new TextField(" Search for recipes or ingredients here...")
  var searchButton: Button = Button("") {
    if (searchBox.text == " Search for recipes or ingredients here...")
      searchBox.text = ""
    p("Notice: Searched: \"" + searchBox.text + "\"")
    changed = true
    leftFeedback.text = "> Return to the previous page, click Back button"
    changeBox(searchBox.text)
    tempSearchText = searchBox.text
  }
  var backButton: Button = Button("") {
    changed = false
    refreshMenuBox()
    leftFeedback.text =
      "> To perform search, type in the box above and click Search button"
    searchBox.text = " Search for recipes or ingredients here..."
    searchBox.foreground = GRAY
    p("Notice: Returned to the main interface")
  }
  var leftFeedback = new TextField("")
  var leftMultifunctionalFrame = new BorderPanel
  var leftMultifunctionalBox = new BoxPanel(Horizontal)
  var leftMultifunctionalText = new TextField("")
  var leftMultifunctionalButton: Button = Button("") {
    addMenuToUI(leftMultifunctionalText.text)
    leftMultifunctionalText.border = createEmptyBorder()
    leftMultifunctionalText.editable = false
    leftMultifunctionalText.text = ""
    if (!changed) refreshMenuBox() else changeBox(searchBox.text)
    outerBox.repaint()
    outerBox.revalidate()
  }
  var rightBox = new BorderPanel
  var rightInfoSection = new BoxPanel(Vertical)
  var rightWelcome = new Label("Options: ")
  var rightCheckboxList: ArrayBuffer[CheckBox] = ArrayBuffer()
  var buttonSave: Button = Button("") {
    fileProcessor.IOWrite()
    p("Notice: Saved")
    leftFeedback.text = "> All changes are saved to saved_data/data.txt "
    leftFeedback.repaint()
  }
  var buttonExit: Button = Button("") { sys.exit(0) }

  // Definitions
  def p[T](a: T) = if (settings.diagnosis) println(a.toString)

  def returnStatus() =
    settings.allAbbreviations zip rightCheckboxList.map(_.selected)

  def updateAllergiesString(): Unit = {
    settings.allergiesString = returnStatus().filter(_._2).map(_._1).mkString
  }

  def revalidateWindow(box: BoxPanel): Unit = {
    leftNormalMenuBox.contents -= box
    if (changed) changeBox(searchBox.text)
    leftNormalMenuBox.repaint()
    leftNormalMenuBox.revalidate()
    outerBox.repaint()
    outerBox.revalidate()
  }

  def refreshMenuBox(): Unit = {
    listenTo(searchBox)
    while (leftNormalMenuBox.contents.nonEmpty)
      leftNormalMenuBox.contents -= leftNormalMenuBox.contents.last
    val food_list_menu = foodList
      .filter(_._1.isMenu)
      .toSeq
      .sortBy(x => menu.checkAvailability(x._1))
      .reverse
      .toMap
    var allergies = (settings.allAbbreviations zip rightCheckboxList.map(
      _.selected
    )).filter(_._2).map(_._1)
    if (allergies.isEmpty) allergies = List[String]()
    val food_list_menu_allergies =
      food_list_menu.filter(x => allergies.forall(y => x._1.tag.contains(y)))
    for ((item_food, item_amount) <- food_list_menu_allergies)
      leftNormalMenuBox.contents += new UISectionBox(item_food, this).defaultBox
    outerBox.repaint()
    outerBox.revalidate()
  }

  def changeBox(keyword: String): Unit = {
    val subUI = new UISearchRepresentation(this, keyword)
    while (leftNormalMenuBox.contents.nonEmpty)
      leftNormalMenuBox.contents -= leftNormalMenuBox.contents.last
    leftNormalMenuBox.contents ++= Array(
      subUI.headlineBorder,
      VStrut(scaleTo(40)),
      subUI.box1Border,
      VStrut(scaleTo(20)),
      subUI.box2Border,
      VStrut(scaleTo(20))
    )
    if (!subUI.keyDouble.isNaN) leftNormalMenuBox.contents += subUI.box3Border
    listenTo(searchBox)
    leftNormalMenuBox.repaint()
    leftMenuScroll.revalidate()
    outerBox.repaint()
    outerBox.revalidate()
  }

  def addMenuToUI(str: String): Unit = {
    try {
      val strList = str.split("\t").map(_.trim)
      p("Input string: " + strList.mkString("\t"))
      if (strList.length != 9) throw new Exception
      val nameAdd: String = strList(0)
      val ingredientsAdd: String = strList(1)
      val firstUnitAdd: String = strList(2).toLowerCase
      val secondUnitAdd: String = strList(3).toLowerCase
      var densityAdd: Double = strList(4).toDouble
      val allergiesAdd: String = strList(5).toUpperCase
      val description_add: String = strList(6)
      val isMenuAdd: Boolean =
        if (strList(7) == "1") true
        else if (strList(7) == "0") false
        else throw new Exception
      var amountAdd: Double = strList(8).toDouble
      if ((!edit) && (menu.returnFoodWithName(nameAdd) != None))
        throw new IOException
      if (amountAdd > 1000) {
        amountAdd = 1000
        println(
          "Notice: The maximum amount allowed in this system is 1000. Your input has been changed to 1000."
        )
      } else if (amountAdd < 0) {
        amountAdd = 0
        println(
          "Notice: The amount cannot be negative. Your input has been changed to 0."
        )
      }
      if (densityAdd < 0) {
        densityAdd = 0
        println(
          "Notice: Density cannot be negative. System changed it to the default value: 0"
        )
      }
      if (ingredientsAdd.isEmpty) {
        val food_add = Food(
          nameAdd,
          scala.collection.mutable.Map[Food, Double](),
          firstUnitAdd,
          secondUnitAdd,
          densityAdd,
          allergiesAdd,
          description_add
        )
        if (isMenuAdd) food_add.setToMenu()
        menu.addFood(food_add, amountAdd)
      } else {
        val ingreMap: collection.mutable.Map[Food, Double] = {
          val itemList = ingredientsAdd.split(",")
          var nameList = ArrayBuffer[Food]()
          var amountList = ArrayBuffer[Double]()
          for (item <- itemList) {
            nameList += menu.returnFoodWithName(item.split("=").head.trim).get
            var tempAmount = item.split("=").last.trim.toDouble
            if (tempAmount > 1000) {
              tempAmount = 1000
              println(
                "Notice: The maximum amount allowed in this system is 1000. The amount of " + item
                  .split("=")
                  .head
                  .trim + "has been changed to 1000."
              )
            } else if (tempAmount < 0) {
              tempAmount = 0
              println(
                "Notice: The amount cannot be negative. The amount of " + item
                  .split("=")
                  .head
                  .trim + "has been changed to 0."
              )
            }
            amountList += tempAmount
          }
          val temp = (nameList zip amountList).toMap
          collection.mutable.Map(temp.toSeq: _*)
        }
        val food_add = Food(
          nameAdd,
          ingreMap,
          firstUnitAdd,
          secondUnitAdd,
          densityAdd,
          allergiesAdd,
          description_add
        )
        if (isMenuAdd) food_add.setToMenu()
        menu.addFood(food_add, amountAdd)
      }
      if (edit) menu.foodList -= editing
      leftFeedback.text = "> Added/Modified successfully!"
    } catch {
      case _: IOException =>
        leftFeedback.text =
          "> Failed. The menu with the same name already exists"
      case _: NoSuchElementException =>
        leftFeedback.text =
          "> Failed. One or more ingredients is missing. Please add ingredients first"
      case _: Exception =>
        leftFeedback.text = "> Failed. Wrong format"
    } finally {
      leftFeedback.repaint()
    }
    searchBox.text =
      if (tempSearchText.isEmpty) " Search for recipes or ingredients here..."
      else tempSearchText
    searchBox.foreground = GRAY
  }

  // Icons
  var iconSelected: ImageIcon = Icon("src/main/scala/icons/selected.png")
  var iconFree: ImageIcon = Icon("src/main/scala/icons/free.png")
  var iconButton: ImageIcon = Icon("src/main/scala/icons/button.png")
  var iconSave: ImageIcon = Icon("src/main/scala/icons/save.png")
  var iconSavePressed: ImageIcon = Icon("src/main/scala/icons/save_done.png")
  var iconExit: ImageIcon = Icon("src/main/scala/icons/exit.png")
  var iconFind: ImageIcon = Icon("src/main/scala/icons/find.png")
  var iconBack: ImageIcon = Icon("src/main/scala/icons/back.png")
  var iconTick: ImageIcon = Icon("src/main/scala/icons/tick.png")

  // Left Welcome Label
  leftWelcome.horizontalAlignment = Left
  leftWelcome.font = new Font("Arial", 0, scaleTo(80))
  leftInfoSection.contents += VStrut(scaleTo(20))
  leftInfoSection.contents += leftWelcome

  // Left Info Box
  leftInfoSection.background = WHITE
  leftInfoSection.border =
    EmptyBorder(scaleTo(30), scaleTo(30), scaleTo(30), scaleTo(30))

  // Left Menu Box ScrollPane Frame
  leftMenuScroll.preferredSize = new Dimension(scaleTo(1440), scaleTo(600))
  leftInfoSection.contents += VStrut(scaleTo(20))
  leftInfoSection.contents += leftMenuScroll

  // Left Menu BoxPanel Normal
  refreshMenuBox()
  leftMenuScroll.contents = leftNormalMenuBox

  // Left Search Area
  leftSearchArea.preferredSize = new Dimension(scaleTo(1440), scaleTo(100))
  leftSearchArea.background = WHITE
  leftInfoSection.contents += VStrut(scaleTo(10))

  // Left Search Prevention TextField (Avoiding cursor move to search box after clicking "MAKE")
  searchPreventionBox.font = new Font("Arial", 0, scaleTo(1))
  searchPreventionBox.border = createEmptyBorder()
  leftInfoSection.contents += searchPreventionBox

  // Left Search TextField
  searchBox.font = new Font("Arial", 0, scaleTo(50))
  searchBox.foreground = GRAY
  searchBox.border = createLineBorder(myColor, scaleTo(5))
  listenTo(searchBox)
  reactions += { case _: FocusGained =>
    p("Notice: Search box gained focus")
    searchBox.text = ""
    searchBox.foreground = BLACK
    outerBox.repaint()
  }
  searchButton.background = WHITE
  searchButton.font = new Font("Arial", 0, scaleTo(50))
  searchButton.preferredSize = new Dimension(scaleTo(100), scaleTo(100))
  searchButton.border = createLineBorder(myColor, scaleTo(5))
  searchButton.icon = iconFind
  backButton.background = WHITE
  backButton.font = new Font("Arial", 0, scaleTo(50))
  backButton.preferredSize = new Dimension(scaleTo(100), scaleTo(100))
  backButton.border = createLineBorder(myColor, scaleTo(5))
  backButton.icon = iconBack
  leftSearchArea.contents += searchBox
  leftSearchArea.contents += searchButton
  leftSearchArea.contents += backButton
  leftInfoSection.contents += leftSearchArea
  leftInfoSection.contents += VStrut(scaleTo(20))

  // Left Real-time feedback TextField
  leftFeedback.preferredSize = new Dimension(scaleTo(200), scaleTo(40))
  leftFeedback.font = new Font("Arial", 0, scaleTo(34))
  leftFeedback.border = createEmptyBorder()
  leftFeedback.editable = false
  leftFeedback.background = WHITE
  leftInfoSection.contents += leftFeedback
  leftInfoSection.contents += VStrut(scaleTo(20))

  // Left Multi-usage Testfield
  leftMultifunctionalText.editable = false
  leftMultifunctionalText.background = WHITE
  leftMultifunctionalText.preferredSize =
    new Dimension(scaleTo(1300), scaleTo(30))
  leftMultifunctionalText.font = new Font("Arial", 0, scaleTo(30))
  leftMultifunctionalText.border = createEmptyBorder()

  // Left Multi-usage Button
  leftMultifunctionalButton.font = new Font("Arial", 0, scaleTo(30))
  leftMultifunctionalButton.border = createEmptyBorder()
  leftMultifunctionalButton.preferredSize =
    new Dimension(scaleTo(50), scaleTo(50))
  leftMultifunctionalButton.background = WHITE
  leftMultifunctionalButton.visible = false
  leftMultifunctionalButton.icon = iconTick
  listenTo(leftMultifunctionalButton)
  reactions += { case _: ButtonClicked =>
    p("Notice: Complete button pressed")
    leftMultifunctionalButton.visible = false
    rightCheckboxList.foreach(_.visible = true)
    buttonSave.visible = true
    leftMultifunctionalButton.repaint()
    leftMultifunctionalButton.revalidate()
    outerBox.repaint()
    outerBox.revalidate()
  }
  // Left multi-usage box
  leftMultifunctionalBox.background = WHITE
  leftMultifunctionalBox.contents ++= Array(
    leftMultifunctionalText,
    HStrut(scaleTo(10)),
    leftMultifunctionalButton
  )

  // Left multi-usage frame
  leftMultifunctionalFrame.preferredSize =
    new Dimension(scaleTo(1440), scaleTo(50))
  leftMultifunctionalFrame.background = WHITE
  leftMultifunctionalFrame.layout(leftMultifunctionalBox) = West
  leftInfoSection.contents += leftMultifunctionalFrame

  // Right Welcome Label
  rightWelcome.horizontalAlignment = Left
  rightWelcome.font = new Font("Arial", 0, scaleTo(64))
  rightWelcome.foreground = WHITE
  rightWelcome.opaque = false

  // Right Checkboxes
  var i = 0
  while (i < settings.allergies.size) {
    var current = new CheckBox(settings.allergies(i))
    current.opaque = false
    current.foreground = WHITE
    current.font = new Font("Arial", 0, scaleTo(50))
    current.selectedIcon = iconSelected
    current.icon = iconFree
    current.iconTextGap = scaleTo(10)
    rightCheckboxList += current
    i += 1
  }
  rightCheckboxList.map(listenTo(_))
  reactions += { case _: ButtonClicked =>
    val allergies = (settings.allAbbreviations zip rightCheckboxList.map(
      _.selected
    )).filter(_._2).map(_._1)
    p(
      "Notice: Checkbox(es) selection changed, new allergen list is: " + allergies
        .mkString("")
    )
    if (!changed) {
      searchBox.text = " Search for recipes or ingredients here..."
      searchBox.foreground = GRAY
      refreshMenuBox()
    } else {
      searchBox.text = tempSearchText
      changeBox(searchBox.text)
    }
    leftMultifunctionalBox.repaint()
    leftMultifunctionalBox.revalidate()
    outerBox.repaint()
    outerBox.revalidate()
  }

  // Right Save Button
  buttonSave.icon = iconSave
  buttonSave.background = WHITE
  buttonSave.opaque = false
  buttonSave.border = createEmptyBorder()
  buttonSave.icon = iconSave
  buttonSave.pressedIcon = iconSavePressed

  // Right Exit Button (Shows only with IOError)
  buttonExit.icon = iconExit
  buttonExit.pressedIcon = iconExit
  buttonExit.background = WHITE
  buttonExit.opaque = false
  buttonExit.border = createEmptyBorder()
  buttonExit.icon = iconExit

  // Right Info Box
  rightInfoSection.contents += rightWelcome
  rightInfoSection.contents += VStrut(scaleTo(30))
  for (checkbox <- rightCheckboxList) {
    rightInfoSection.contents += checkbox
    rightInfoSection.contents += VStrut(scaleTo(20))
  }
  rightInfoSection.contents += VStrut(scaleTo(200))
  rightInfoSection.contents += buttonSave
  rightInfoSection.background = myColor
  rightInfoSection.border =
    EmptyBorder(scaleTo(20), scaleTo(20), scaleTo(20), scaleTo(20))

  // Frame Section
  outerBox.contents += leftBox
  outerBox.contents += rightBox

  // Left Panel Section
  leftBox.preferredSize = new Dimension(scaleTo(1440), scaleTo(1080))
  leftBox.layout(leftInfoSection) = North
  leftBox.background = WHITE
  contents = outerBox

  // Right Panel Section
  rightBox.preferredSize = new Dimension(scaleTo(480), scaleTo(1080))
  rightBox.layout(rightInfoSection) = North
  rightBox.background = myColor

  // Load file
  fileProcessor.loadFromIO()
}

object GUI extends App {
  private def main(): Unit = {
    val ui = new UI
    ui.visible = true
  }
  this.main()
}
