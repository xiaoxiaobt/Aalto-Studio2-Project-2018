package cook
import scala.swing._
import scala.swing.BorderPanel.Position.{West, North}
import scala.swing.Orientation.{Horizontal, Vertical}
import scala.swing.Alignment.Left
import scala.swing.event._
import scala.collection.mutable.ArrayBuffer
import java.awt.Color.{BLACK, GRAY, RED, WHITE}
import javax.swing.BorderFactory.{createEmptyBorder, createLineBorder}
import javax.swing.ImageIcon
import Swing.{Icon, VStrut, HStrut, EmptyBorder}

class UI extends MainFrame {
  title = "Smart Cookbook"
  preferredSize = new Dimension(1920, 1080)

  // Initialize
  val menu: FoodMenu = new FoodMenu()
  val myColor = Settings.color
  var changed = false
  private var tempSearchText = ""
  private val fileProcessor = new FileProcessor(this)

  // Frames, boxes and buttons (Almost all boxes)
  val outerBox = new BoxPanel(Horizontal)
  val leftBox = new BorderPanel
  val leftInfoSection = new BoxPanel(Vertical)
  val leftWelcome = new Label("What would you like to eat today? ")
  leftWelcome.horizontalAlignment = Left
  val leftMenuScroll = new ScrollPane()
  val leftNormalMenuBox = new BoxPanel(Vertical)
  val leftSearchArea = new BoxPanel(Horizontal)
  val searchPreventionBox = new TextField("")
  val searchBox = new TextField(" Search for recipes or ingredients here...")
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
  val leftFeedback = new TextField("")
  val leftMultifunctionalFrame = new BorderPanel
  val leftMultifunctionalBox = new BoxPanel(Horizontal)
  val leftMultifunctionalText = new TextField("")
  val leftMultifunctionalButton: Button = Button("") {
    addMenuToUI(leftMultifunctionalText.text)
    leftMultifunctionalText.border = createEmptyBorder()
    leftMultifunctionalText.editable = false
    leftMultifunctionalText.text = ""
    if (!changed) refreshMenuBox() else changeBox(searchBox.text)
    outerBox.repaint()
    outerBox.revalidate()
  }
  val rightBox = new BorderPanel
  val rightInfoSection = new BoxPanel(Vertical)
  val rightWelcome = new Label("Options: ")
  val rightCheckboxList: ArrayBuffer[CheckBox] = ArrayBuffer()
  var buttonSave: Button = Button("") {
    fileProcessor.IOWritelines()
    p("Notice: Saved")
    leftFeedback.text = "> All changes are saved to saved_data/data.txt "
    leftFeedback.repaint()
  }
  val buttonExit: Button = Button("") { sys.exit(0) }

  // Definitions
  def p[T](a: T) = if (Settings.diagnosis) println(a.toString)

  def returnStatus() =
    Settings.allAbbreviations zip rightCheckboxList.map(_.selected)

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
    val foodListMenu = menu.foodMap
      .filter(_._1.isMenu)
      .toArray
      .sortBy(x => menu.checkAvailability(x._1))
      .reverse
    val allergies = (Settings.allAbbreviations zip rightCheckboxList.map(
      _.selected
    )).filter(_._2).map(_._1)
    val foodListMenuAllergies =
      foodListMenu
        .filter(x => allergies.forall(y => x._1.tag.contains(y)))
        .map(_._1)
    for (food <- foodListMenuAllergies)
      leftNormalMenuBox.contents += new UISectionBox(food, this).defaultBox
    outerBox.repaint()
    outerBox.revalidate()
  }

  def changeBox(keyword: String): Unit = {
    val subUI = new UISearchRepresentation(this, keyword.trim)
    while (leftNormalMenuBox.contents.nonEmpty)
      leftNormalMenuBox.contents -= leftNormalMenuBox.contents.last
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
    leftNormalMenuBox.repaint()
    leftMenuScroll.revalidate()
    outerBox.repaint()
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
    leftFeedback.repaint()
    searchBox.text =
      if (tempSearchText.isEmpty) " Search for recipes or ingredients here..."
      else tempSearchText
    searchBox.foreground = GRAY
  }

  // Icons
  private val iconSelected: ImageIcon = Icon(
    "src/main/scala/icons/selected.png"
  )
  private val iconFree: ImageIcon = Icon("src/main/scala/icons/free.png")
  private val iconButton: ImageIcon = Icon("src/main/scala/icons/button.png")
  private val iconSave: ImageIcon = Icon("src/main/scala/icons/save.png")
  private val iconSavePressed: ImageIcon = Icon(
    "src/main/scala/icons/save_done.png"
  )
  private val iconExit: ImageIcon = Icon("src/main/scala/icons/exit.png")
  private val iconFind: ImageIcon = Icon("src/main/scala/icons/find.png")
  private val iconBack: ImageIcon = Icon("src/main/scala/icons/back.png")
  private val iconTick: ImageIcon = Icon("src/main/scala/icons/tick.png")

  // Left Welcome Label
  leftWelcome.horizontalAlignment = Left
  leftWelcome.font = new Font("Arial", 0, 80)
  leftInfoSection.contents += VStrut(20)
  leftInfoSection.contents += leftWelcome

  // Left Info Box
  leftInfoSection.background = WHITE
  leftInfoSection.border = EmptyBorder(30, 0, 30, 30)

  // Left Menu Box ScrollPane Frame
  leftMenuScroll.preferredSize = new Dimension(1440, 600)
  leftInfoSection.contents += VStrut(20)
  leftInfoSection.contents += leftMenuScroll

  // Left Menu BoxPanel Normal
  refreshMenuBox()
  leftMenuScroll.contents = leftNormalMenuBox

  // Left Search Area
  leftSearchArea.preferredSize = new Dimension(1440, 100)
  leftSearchArea.background = WHITE
  leftInfoSection.contents += VStrut(10)

  // Left Search Prevention TextField (Avoiding cursor move to search box after clicking "MAKE")
  searchPreventionBox.font = new Font("Arial", 0, 1)
  searchPreventionBox.border = createEmptyBorder()
  leftInfoSection.contents += searchPreventionBox

  // Left Search TextField
  searchBox.font = new Font("Arial", 0, 50)
  searchBox.foreground = GRAY
  searchBox.border = createLineBorder(myColor, 5)
  listenTo(searchBox)
  reactions += { case _: FocusGained =>
    p("Notice: Search box gained focus")
    searchBox.text = ""
    searchBox.foreground = BLACK
    outerBox.repaint()
  }
  searchButton.background = WHITE
  searchButton.font = new Font("Arial", 0, 50)
  searchButton.preferredSize = new Dimension(100, 100)
  searchButton.border = createLineBorder(myColor, 5)
  searchButton.icon = iconFind
  backButton.background = WHITE
  backButton.font = new Font("Arial", 0, 50)
  backButton.preferredSize = new Dimension(100, 100)
  backButton.border = createLineBorder(myColor, 5)
  backButton.icon = iconBack
  leftSearchArea.contents += searchBox
  leftSearchArea.contents += searchButton
  leftSearchArea.contents += backButton
  leftInfoSection.contents += leftSearchArea
  leftInfoSection.contents += VStrut(20)

  // Left Real-time feedback TextField
  leftFeedback.preferredSize = new Dimension(200, 40)
  leftFeedback.font = new Font("Arial", 0, 34)
  leftFeedback.border = createEmptyBorder()
  leftFeedback.editable = false
  leftFeedback.background = WHITE
  leftInfoSection.contents += leftFeedback
  leftInfoSection.contents += VStrut(20)

  // Left Multi-usage Textfield
  leftMultifunctionalText.editable = false
  leftMultifunctionalText.background = WHITE
  leftMultifunctionalText.preferredSize = new Dimension(1300, 30)
  leftMultifunctionalText.font = new Font("Arial", 0, 30)
  leftMultifunctionalText.border = createEmptyBorder()

  // Left Multi-usage Button
  leftMultifunctionalButton.font = new Font("Arial", 0, 30)
  leftMultifunctionalButton.border = createEmptyBorder()
  leftMultifunctionalButton.preferredSize = new Dimension(50, 50)
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
    HStrut(10),
    leftMultifunctionalButton
  )

  // Left multi-usage frame
  leftMultifunctionalFrame.preferredSize = new Dimension(1440, 50)
  leftMultifunctionalFrame.background = WHITE
  leftMultifunctionalFrame.layout(leftMultifunctionalBox) = West
  leftInfoSection.contents += leftMultifunctionalFrame

  // Right Welcome Label
  rightWelcome.horizontalAlignment = Left
  rightWelcome.font = new Font("Arial", 0, 64)
  rightWelcome.foreground = WHITE
  rightWelcome.opaque = false

  // Right Checkboxes
  for (a <- Settings.allergies) {
    val current = new CheckBox(a)
    current.opaque = false
    current.foreground = WHITE
    current.font = new Font("Arial", 0, 50)
    current.selectedIcon = iconSelected
    current.icon = iconFree
    current.iconTextGap = 10
    rightCheckboxList += current
  }

  rightCheckboxList.map(listenTo(_))
  reactions += { case _: ButtonClicked =>
    val allergies = (Settings.allAbbreviations zip rightCheckboxList.map(
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
    leftMultifunctionalBox.revalidate()
    leftMultifunctionalBox.repaint()
    outerBox.revalidate()
    outerBox.repaint()
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
  outerBox.contents += leftBox
  outerBox.contents += rightBox

  // Left Panel Section
  leftBox.preferredSize = new Dimension(1440, 1080)
  leftBox.layout(leftInfoSection) = North
  leftBox.background = WHITE
  contents = outerBox

  // Right Panel Section
  rightBox.preferredSize = new Dimension(480, 1080)
  rightBox.layout(rightInfoSection) = North
  rightBox.background = myColor

  // Load file
  fileProcessor.linesToUI()
  // Repaint and revalidate
  refreshMenuBox()

  this.visible = true
}

object UI extends App {
  val ui = new UI
}
