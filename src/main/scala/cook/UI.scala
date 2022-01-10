package cook
import scala.swing.*
import scala.swing.event.*
import scala.swing.Alignment.Left
import scala.swing.BorderPanel.Position.{West, North}
import scala.swing.Orientation.{Horizontal, Vertical}
import scala.swing.Swing.{
  Icon,
  VStrut,
  HStrut,
  EmptyBorder,
  LineBorder,
  EmptyIcon,
  pair2Dimension
}
import scala.collection.mutable.ArrayBuffer
import java.awt.Color.{BLACK, GRAY, RED, WHITE}

class UI extends MainFrame {
  title = "Smart Cookbook"
  preferredSize = (1920, 1080)

  // Initialize
  val menu: FoodMenu = FoodMenu()
  val myColor = Settings.color
  var changed = false
  private var tempSearchText = ""
  private val fileProcessor = FileProcessor(menu)

  // Frames, boxes and buttons (Almost all boxes)
  val outerBox = BoxPanel(Horizontal)
  val leftBox = BorderPanel()
  val leftInfoSection = BoxPanel(Vertical)
  val leftWelcome = Label("What would you like to eat today?", EmptyIcon, Left)
  val leftMenuScroll = ScrollPane()
  val leftNormalMenuBox = BoxPanel(Vertical)
  val leftSearchArea = BoxPanel(Horizontal)
  val searchPreventionBox = TextField("")
  val searchBox = TextField(" Search for recipes or ingredients here...")
  val searchButton: Button = Button("") {
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
  val leftFeedback = TextField("")
  val leftMultifunctionalFrame = BorderPanel()
  val leftMultifunctionalBox = BoxPanel(Horizontal)
  val leftMultifunctionalText = TextField("")
  val leftMultifunctionalButton: Button = Button("") {
    addMenuToUI(leftMultifunctionalText.text)
    leftMultifunctionalText.border = EmptyBorder
    leftMultifunctionalText.editable = false
    leftMultifunctionalText.text = ""
    if (!changed) refreshMenuBox() else changeBox(searchBox.text)
    outerBox.revalidate()
  }
  val rightBox = BorderPanel()
  val rightInfoSection = BoxPanel(Vertical)
  val rightWelcome = Label("Options: ", EmptyIcon, Left)
  val rightCheckboxList: ArrayBuffer[CheckBox] = ArrayBuffer()
  var buttonSave: Button = Button("") {
    fileProcessor.IOWritelines()
    p("Notice: Saved")
    leftFeedback.text = "> All changes are saved to saved_data/data.txt "
  }
  val buttonExit: Button = Button("") { sys.exit(0) }

  // Definitions
  def p[T](a: T) = if (Settings.diagnosis) println(a.toString)

  def returnStatus() =
    Settings.allAbbreviations
      .zip(rightCheckboxList.map(_.selected))
      .filter(_._2)
      .map(_._1)

  def revalidateWindow(box: BoxPanel): Unit = {
    leftNormalMenuBox.contents -= box
    if (changed) changeBox(searchBox.text)
    leftNormalMenuBox.revalidate()
    outerBox.revalidate()
  }

  def refreshMenuBox(): Unit = {
    listenTo(searchBox)
    leftNormalMenuBox.contents.clear()
    val foodListMenu = menu.foodMap
      .filter(_._1.isMenu)
      .toArray
      .sortBy(x => menu.checkAvailability(x._1))
      .reverse
    val allergies = returnStatus()
    val foodListMenuAllergies =
      foodListMenu
        .filter(x => allergies.forall(y => x._1.tag.contains(y)))
        .map(_._1)
    for (food <- foodListMenuAllergies)
      leftNormalMenuBox.contents += UISectionBox(food, this)
    outerBox.revalidate()
  }

  def changeBox(keyword: String): Unit = {
    val subUI = UISearchRepresentation(this, keyword.trim)
    leftNormalMenuBox.contents.clear()
    leftNormalMenuBox.contents ++= Array(
      subUI.headlineBorder,
      VStrut(40),
      subUI.box1Border,
      VStrut(20),
      subUI.box2Border,
      VStrut(20)
    )
    if (keyword.trim.toDoubleOption.isDefined)
      leftNormalMenuBox.contents += subUI.box3Border
    listenTo(searchBox)
    leftMenuScroll.revalidate()
    outerBox.revalidate()
  }

  def addMenuToUI(str: String): Unit = {
    val holder = fileProcessor.lineParser(str)
    if (holder.isDefined) {
      val foodOption = menu.getFoodArray.find(x => x.name == holder.get.name)
      if (foodOption.isDefined) {
        menu.foodMap -= foodOption.get
      }
      fileProcessor.linesToUI(Array(str))
    } else {
      leftFeedback.text = "> Failed. Wrong format"
    }
    searchBox.text =
      if (tempSearchText.isEmpty) " Search for recipes or ingredients here..."
      else tempSearchText
    searchBox.foreground = GRAY
  }

  // Icons
  private val iconSelected = Icon("src/main/scala/icons/selected.png")
  private val iconFree = Icon("src/main/scala/icons/free.png")
  private val iconButton = Icon("src/main/scala/icons/button.png")
  private val iconSave = Icon("src/main/scala/icons/save.png")
  private val iconSavePressed = Icon("src/main/scala/icons/save_done.png")
  private val iconExit = Icon("src/main/scala/icons/exit.png")
  private val iconFind = Icon("src/main/scala/icons/find.png")
  private val iconBack = Icon("src/main/scala/icons/back.png")
  private val iconTick = Icon("src/main/scala/icons/tick.png")

  // Left Welcome Label
  leftWelcome.font = Font("Arial", Font.Plain, 80)
  leftInfoSection.contents += VStrut(20)
  leftInfoSection.contents += leftWelcome

  // Left Info Box
  leftInfoSection.background = WHITE
  leftInfoSection.border = EmptyBorder(30, 0, 30, 30)

  // Left Menu Box ScrollPane Frame
  leftMenuScroll.preferredSize = (1440, 600)
  leftInfoSection.contents += VStrut(20)
  leftInfoSection.contents += leftMenuScroll

  // Left Menu BoxPanel Normal
  refreshMenuBox()
  leftMenuScroll.contents = leftNormalMenuBox

  // Left Search Area
  leftSearchArea.preferredSize = (1440, 100)
  leftSearchArea.background = WHITE
  leftInfoSection.contents += VStrut(10)

  // Left Search Prevention TextField
  // (Avoiding cursor move to search box after clicking "MAKE")
  searchPreventionBox.font = Font("Arial", Font.Plain, 1)
  searchPreventionBox.border = EmptyBorder
  leftInfoSection.contents += searchPreventionBox

  // Left Search TextField
  searchBox.font = Font("Arial", Font.Plain, 50)
  searchBox.foreground = GRAY
  searchBox.border = LineBorder(myColor, 5)
  listenTo(searchBox)
  reactions += { case _: FocusGained =>
    p("Notice: Search box gained focus")
    searchBox.text = ""
    searchBox.foreground = BLACK
  }
  searchButton.background = WHITE
  searchButton.font = Font("Arial", Font.Plain, 50)
  searchButton.preferredSize = (100, 100)
  searchButton.border = LineBorder(myColor, 5)
  searchButton.icon = iconFind
  backButton.background = WHITE
  backButton.font = Font("Arial", Font.Plain, 50)
  backButton.preferredSize = (100, 100)
  backButton.border = LineBorder(myColor, 5)
  backButton.icon = iconBack
  leftSearchArea.contents += searchBox
  leftSearchArea.contents += searchButton
  leftSearchArea.contents += backButton
  leftInfoSection.contents += leftSearchArea
  leftInfoSection.contents += VStrut(20)

  // Left Real-time feedback TextField
  leftFeedback.preferredSize = (200, 40)
  leftFeedback.font = Font("Arial", Font.Plain, 34)
  leftFeedback.border = EmptyBorder
  leftFeedback.editable = false
  leftFeedback.background = WHITE
  leftInfoSection.contents += leftFeedback
  leftInfoSection.contents += VStrut(20)

  // Left Multi-usage Textfield
  leftMultifunctionalText.editable = false
  leftMultifunctionalText.background = WHITE
  leftMultifunctionalText.preferredSize = (1300, 30)
  leftMultifunctionalText.font = Font("Arial", Font.Plain, 30)
  leftMultifunctionalText.border = EmptyBorder

  // Left Multi-usage Button
  leftMultifunctionalButton.font = Font("Arial", Font.Plain, 30)
  leftMultifunctionalButton.border = EmptyBorder
  leftMultifunctionalButton.preferredSize = (50, 50)
  leftMultifunctionalButton.background = WHITE
  leftMultifunctionalButton.visible = false
  leftMultifunctionalButton.icon = iconTick
  listenTo(leftMultifunctionalButton)
  reactions += { case _: ButtonClicked =>
    p("Notice: Complete button pressed")
    leftMultifunctionalButton.visible = false
    rightCheckboxList.foreach(_.visible = true)
    buttonSave.visible = true
    leftMultifunctionalButton.revalidate()
    outerBox.revalidate()
  }

  // Left multi-usage box
  leftMultifunctionalBox.background = WHITE
  leftMultifunctionalBox.contents ++= Array(
    leftMultifunctionalText,
    HStrut(10),
    leftMultifunctionalButton
  )

  // Left multi-usage frame
  leftMultifunctionalFrame.preferredSize = (1440, 50)
  leftMultifunctionalFrame.background = WHITE
  leftMultifunctionalFrame.layout(leftMultifunctionalBox) = West
  leftInfoSection.contents += leftMultifunctionalFrame

  // Right Welcome Label
  rightWelcome.font = Font("Arial", Font.Plain, 64)
  rightWelcome.foreground = WHITE
  rightWelcome.opaque = false

  // Right Checkboxes
  for (a <- Settings.allergies) {
    val current = CheckBox(a)
    current.opaque = false
    current.foreground = WHITE
    current.font = Font("Arial", Font.Plain, 50)
    current.selectedIcon = iconSelected
    current.icon = iconFree
    current.iconTextGap = 10
    rightCheckboxList += current
  }

  rightCheckboxList.map(listenTo(_))
  reactions += { case _: ButtonClicked =>
    val allergies = returnStatus()
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
    leftMultifunctionalBox.revalidate()
    outerBox.revalidate()
  }

  // Right Save Button
  buttonSave.icon = iconSave
  buttonSave.background = WHITE
  buttonSave.opaque = false
  buttonSave.border = EmptyBorder
  buttonSave.icon = iconSave
  buttonSave.pressedIcon = iconSavePressed

  // Right Exit Button (Shows only with IOError)
  buttonExit.icon = iconExit
  buttonExit.background = WHITE
  buttonExit.opaque = false
  buttonExit.border = EmptyBorder
  buttonExit.icon = iconExit
  buttonExit.pressedIcon = iconExit

  // Right Info Box
  rightInfoSection.contents += rightWelcome
  rightInfoSection.contents += VStrut(30)
  for (checkbox <- rightCheckboxList) {
    rightInfoSection.contents += checkbox
    rightInfoSection.contents += VStrut(20)
  }
  rightInfoSection.contents += VStrut(200)
  rightInfoSection.contents += buttonSave
  rightInfoSection.background = myColor
  rightInfoSection.border = EmptyBorder(20, 20, 20, 20)

  // Frame Section
  contents = outerBox
  outerBox.contents += leftBox
  outerBox.contents += rightBox

  // Left Panel Section
  leftBox.preferredSize = (1440, 1080)
  leftBox.layout(leftInfoSection) = North
  leftBox.background = WHITE

  // Right Panel Section
  rightBox.preferredSize = (480, 1080)
  rightBox.layout(rightInfoSection) = North
  rightBox.background = myColor

  // Load file
  private val (lines, status) = fileProcessor.IOReadlines()
  fileProcessor.linesToUI(lines)
  leftFeedback.text = status

  // Revalidate
  refreshMenuBox()

  this.visible = true
}

object UI extends App {
  val ui = UI()
}
