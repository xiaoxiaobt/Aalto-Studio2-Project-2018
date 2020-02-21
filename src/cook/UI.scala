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
import java.awt.Color.{ BLACK, WHITE, GRAY, RED }
import javax.swing.ImageIcon
import javax.swing.BorderFactory.{ createEmptyBorder, createLineBorder }
import Swing._
import Settings.scaleTo

class UI extends MainFrame {
  title = "Smart Cookbook"
  preferredSize = new Dimension(scaleTo(1920), scaleTo(1080))

  // Initialize
  var menu = new Menu()
  var fridge = menu.fridge
  var settings = Settings
  def foodList = fridge.foodList
  var myColor = settings.color
  var changed = false
  var edit = false
  var editing: Food = null
  var tempSearchText = ""
  var fileProcessor = new FileProcessor(this)

  // Frames, boxes and buttons (Almost all boxes)
  var outerBox = new BoxPanel(Horizontal)
  var leftBox = new BorderPanel
  var leftInfoSection = new BoxPanel(Vertical)
  var leftWelcome = new Label("What would you like to eat today? ")
  var leftMenuScroll = new ScrollPane()
  var leftNormalMenuBox = new BoxPanel(Vertical)
  var leftSearchArea = new BoxPanel(Horizontal)
  var searchPreventionBox = new TextField("")
  var searchBox = new TextField(" Search for recipes or ingredients here...")
  var searchButton = Button("") {
    if (searchBox.text == " Search for recipes or ingredients here...") searchBox.text = ""
    p("Notice: Searched: \"" + searchBox.text + "\"")
    changed = true
    leftFeedback.text = "> Return to the previous page, click Back button"
    changeBox(searchBox.text)
    tempSearchText = searchBox.text
  }
  var backButton = Button("") {
    changed = false
    refreshMenuBox()
    leftFeedback.text = "> To perform search, type in the box above and click Search button"
    searchBox.text = " Search for recipes or ingredients here..."
    searchBox.foreground = GRAY
    p("Notice: Returned to the main interface")
  }
  var leftFeedback = new TextField("")
  var leftMultifunctionalFrame = new BorderPanel
  var leftMultifunctionalBox = new BoxPanel(Horizontal)
  var leftMultifunctionalText = new TextField("")
  var leftMultifunctionalButton = Button("") {
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
  var rightCheckboxList = ArrayBuffer[CheckBox]()
  var buttonSave = Button("") {
    fileProcessor.IOWrite()
    p("Notice: Saved")
    leftFeedback.text = "> All changes are saved to src/saved_data/data.txt "
    leftFeedback.repaint()
  }
  var buttonExit = Button("") { sys.exit(0) }

  // Definitions
  def p[T](a: T) = if (settings.diagnosis) println(a.toString)

  def returnStatus() = (settings.all_abbri zip rightCheckboxList.map(_.selected))

  def updateAllergiesString() = {
    settings.allergiesString = returnStatus().filter(_._2).map(_._1).mkString
  }
  def revalidateWindow(box: BoxPanel) = {
    leftNormalMenuBox.contents -= box
    if (changed) changeBox(searchBox.text)
    leftNormalMenuBox.repaint()
    leftNormalMenuBox.revalidate()
    outerBox.repaint()
    outerBox.revalidate()
  }
  def refreshMenuBox() = {
    listenTo(searchBox)
    while (leftNormalMenuBox.contents.nonEmpty) {
      leftNormalMenuBox.contents -= leftNormalMenuBox.contents.last
    }
    var food_list_menu = foodList.filter(_._1.isMenu).toSeq.sortBy(x => menu.checkAvailability(x._1)).reverse.toMap
    var allergies = (settings.all_abbri zip rightCheckboxList.map(_.selected)).filter(_._2).map(_._1)
    if (allergies.isEmpty) allergies = List[String]()
    var food_list_menu_allergies = food_list_menu.filter(x => allergies.forall(y => x._1.tag.contains(y)))
    for ((item_food, item_amount) <- food_list_menu_allergies) {
      leftNormalMenuBox.contents += new UISectionBox(item_food, this).default_box
    }
    outerBox.repaint()
    outerBox.revalidate()
  }
  def changeBox(keyword: String) = {
    var sub_ui = new UISearchRepresentation(this, keyword)
    while (leftNormalMenuBox.contents.nonEmpty) {
      leftNormalMenuBox.contents -= leftNormalMenuBox.contents.last
    }
    leftNormalMenuBox.contents += sub_ui.headlineBorder
    leftNormalMenuBox.contents += VStrut(scaleTo(40))
    leftNormalMenuBox.contents += sub_ui.box1Border
    leftNormalMenuBox.contents += VStrut(scaleTo(20))
    leftNormalMenuBox.contents += sub_ui.box2Border
    leftNormalMenuBox.contents += VStrut(scaleTo(20))
    if (!sub_ui.keyDouble.isNaN) leftNormalMenuBox.contents += sub_ui.box3Border
    listenTo(searchBox)
    leftNormalMenuBox.repaint()
    leftMenuScroll.revalidate()
    outerBox.repaint()
    outerBox.revalidate()
  }
  def addMenuToUI(str: String) = {
    try {
      var str_list = str.split("\t").map(_.trim())
      p("Input string: " + str_list.mkString("\t"))
      if (str_list.size != 9) throw new Exception
      var name_add: String = str_list(0)
      var ingredients_add: String = str_list(1)
      var first_unit_add: String = str_list(2).toLowerCase
      var second_unit_add: String = str_list(3).toLowerCase
      var density_add: Double = str_list(4).toDouble
      var alleriges_add: String = str_list(5).toUpperCase
      var description_add: String = str_list(6)
      var isMenu_add: Boolean = if (str_list(7) == "1") true else if (str_list(7) == "0") false else throw new Exception
      var amount_add: Double = str_list(8).toDouble
      if ((!edit) && (menu.returnFoodWithName(name_add) != None)) throw new IOException
      if (amount_add > 1000) {
        amount_add = 1000
        println("Notice: The maximum amount allowed in this system is 1000. Your input has been changed to 1000.")
      } else if (amount_add < 0) {
        amount_add = 0
        println("Notice: The amount cannot be negative. Your input has been changed to 0.")
      }
      if (density_add < 0) {
        density_add = 0
        println("Notice: Density cannot be negative. System changed it to the default value: 0")
      }
      if (ingredients_add.isEmpty) {
        var food_add = new Food(name_add, scala.collection.mutable.Map[Food, Double](), first_unit_add, second_unit_add, density_add, alleriges_add, description_add)
        if (isMenu_add) food_add.setToMenu()
        fridge.addFood(food_add, amount_add)
      } else {
        var ingre_map: collection.mutable.Map[Food, Double] = {
          var item_list = ingredients_add.split(",")
          var name_list = ArrayBuffer[Food]()
          var amount_list = ArrayBuffer[Double]()
          for (item <- item_list) {
            name_list += menu.returnFoodWithName(item.split("=").head.trim).get
            var temp_amount = item.split("=").last.trim.toDouble
            if (temp_amount > 1000) {
              temp_amount = 1000
              println("Notice: The maximum amount allowed in this system is 1000. The amount of " + item.split("=").head.trim + "has been changed to 1000.")
            } else if (temp_amount < 0) {
              temp_amount = 0
              println("Notice: The amount cannot be negative. The amount of " + item.split("=").head.trim + "has been changed to 0.")
            }
            amount_list += temp_amount
          }
          var temp = (name_list zip amount_list).toMap
          collection.mutable.Map(temp.toSeq: _*)
        }
        var food_add = new Food(name_add, ingre_map, first_unit_add, second_unit_add, density_add, alleriges_add, description_add)
        if (isMenu_add) food_add.setToMenu()
        fridge.addFood(food_add, amount_add)
      }
      if (edit) fridge.foodList -= editing
      leftFeedback.text = "> Added/Modified successfully!"
    } catch {
      case e: IOException => {
        leftFeedback.text = "> Failed. The menu with the same name already exists"
      }
      case e: NoSuchElementException => {
        leftFeedback.text = "> Failed. One or more ingredients is missing. Please add ingredients first"
      }
      case e: Exception => {
        leftFeedback.text = "> Failed. Wrong format"
      }
    } finally {
      leftFeedback.repaint()
    }
    searchBox.text = if (tempSearchText.isEmpty) " Search for recipes or ingredients here..." else tempSearchText
    searchBox.foreground = GRAY
  }

  // Icons
  var iconSelected = new ImageIcon("src/icons/selected.png")
  var iconFree = new ImageIcon("src/icons/free.png")
  var iconButton = new ImageIcon("src/icons/button.png")
  var iconSave = new ImageIcon("src/icons/save.png")
  var iconSavePressed = new ImageIcon("src/icons/save_done.png")
  var iconExit = new ImageIcon("src/icons/exit.png")
  var iconFind = new ImageIcon("src/icons/find.png")
  var iconBack = new ImageIcon("src/icons/back.png")
  var iconTick = new ImageIcon("src/icons/tick.png")

  // Left Welcome Label
  leftWelcome.horizontalAlignment = Left
  leftWelcome.font = new Font("Arial", 0, scaleTo(80))
  leftInfoSection.contents += VStrut(scaleTo(20))
  leftInfoSection.contents += leftWelcome

  // Left Info Box
  leftInfoSection.background = WHITE
  leftInfoSection.border = EmptyBorder(scaleTo(30), scaleTo(30), scaleTo(30), scaleTo(30))

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
  reactions += {
    case e: FocusGained => {
      p("Notice: Search box gained focus")
      searchBox.text = ""
      searchBox.foreground = BLACK
      outerBox.repaint()
    }
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
  leftMultifunctionalText.preferredSize = new Dimension(scaleTo(1300), scaleTo(30))
  leftMultifunctionalText.font = new Font("Arial", 0, scaleTo(30))
  leftMultifunctionalText.border = createEmptyBorder()

  // Left Muti-usage Button
  leftMultifunctionalButton.font = new Font("Arial", 0, scaleTo(30))
  leftMultifunctionalButton.border = createEmptyBorder()
  leftMultifunctionalButton.preferredSize = new Dimension(scaleTo(50), scaleTo(50))
  leftMultifunctionalButton.background = WHITE
  leftMultifunctionalButton.visible = false
  leftMultifunctionalButton.icon = iconTick
  listenTo(leftMultifunctionalButton)
  reactions += {
    case e: ButtonClicked => {
      p("Notice: Complete button pressed")
      leftMultifunctionalButton.visible = false
      rightCheckboxList.foreach(_.visible = true)
      buttonSave.visible = true
      leftMultifunctionalButton.repaint()
      leftMultifunctionalButton.revalidate()
      outerBox.repaint()
      outerBox.revalidate()
    }
  }

  // Left multi-usage box
  leftMultifunctionalBox.background = WHITE
  leftMultifunctionalBox.contents += leftMultifunctionalText
  leftMultifunctionalBox.contents += HStrut(scaleTo(10))
  leftMultifunctionalBox.contents += leftMultifunctionalButton

  // Left multi-usage frame
  leftMultifunctionalFrame.preferredSize = new Dimension(scaleTo(1440), scaleTo(50))
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
  reactions += {
    case e: ButtonClicked => {
      var allergies = (settings.all_abbri zip rightCheckboxList.map(_.selected)).filter(_._2).map(_._1)
      p("Notice: Checkbox(es) selection changed, new allergen list is: " + allergies.mkString(""))
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
  rightInfoSection.border = EmptyBorder(scaleTo(20), scaleTo(20), scaleTo(20), scaleTo(20))

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

object UI extends App {
  private def main() {
    val ui = new UI
    ui.visible = true
  }
  this.main()
}