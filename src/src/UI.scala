package src
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

class Holder(name_add: String, ingre_map: Map[String, Double], first_unit_add: String, second_unit_add: String, density_add: Double, alleriges_add: String, description_add: String, isMenu_add: Boolean, amount_add: Double) {
  var name = name_add
  var ingre = ingre_map
  var first_u = first_unit_add
  var second_u = second_unit_add
  var density = density_add
  var aller = alleriges_add
  var description = description_add
  var boo = isMenu_add
  var amount = amount_add 
}

class UI extends MainFrame {
  title = "Smart Cookbook"
  preferredSize = new Dimension(1920, 1080)

  // Initialize
  var menu = new Menu()
  var fridge = menu.fridge
  var settings = fridge.settings
  def food_list = fridge.food_list
  var my_color = settings.color
  var changed = 0
  var edit = 0
  var editing: Food = null
  var temp_search_text = ""

  // Frames, boxes and buttons (Almost all boxes)
  var outer_box = new BoxPanel(Horizontal)
  var left_box = new BorderPanel
  var left_info_section = new BoxPanel(Vertical)
  var left_welcome = new Label("What would you like to eat today? ")
  var left_menu_scroll = new ScrollPane()
  var left_normal_menu_box = new BoxPanel(Vertical)
  var left_search_area = new BoxPanel(Horizontal)
  var search_prevention_box = new TextField("")
  var search_box = new TextField(" Search for recipes or ingredients here...")
  var search_button = Button("") {
    if (search_box.text == " Search for recipes or ingredients here...") search_box.text = ""
    p("Notice: Searched: \"" + search_box.text + "\"")
    changed = 1
    left_feedback.text = "> Return to the previous page, click Back button"
    change_box(search_box.text)
    temp_search_text = search_box.text
  }
  var back_button = Button("") {
    changed = 0
    refresh_menu_box()
    left_feedback.text = "> To perform search, type in the box above and click Search button"
    search_box.text = " Search for recipes or ingredients here..."
    search_box.foreground = GRAY
    p("Notice: Returned to the main interface")
  }
  var left_feedback = new TextField("")
  var left_multi_frame = new BorderPanel
  var left_multi_box = new BoxPanel(Horizontal)
  var left_multi_text = new TextField("")
  var left_multi_button = Button("") {
    add_menu_to_ui(left_multi_text.text)
    left_multi_text.border = BorderFactory.createEmptyBorder()
    left_multi_text.editable = false
    left_multi_text.text = ""
    if (changed == 0) refresh_menu_box() else change_box(search_box.text)
    button_save.visible = true
    outer_box.repaint()
    outer_box.revalidate()
  }
  var right_box = new BorderPanel
  var right_info_section = new BoxPanel(Vertical)
  var right_welcome = new Label("Options: ")
  var right_checkbox_list = ArrayBuffer[CheckBox]()
  var button_save = Button("") {
    IOWrite()
    p("Notice: Saved")
    left_feedback.text = "> All changes are saved to src/saved_data/data.txt "
    left_feedback.repaint()
  }
  var button_exit = Button("") { sys.exit(0) }

  //IO Write Function
  def IOWrite() = {
    var file = new File("src/saved_data/data.txt")
    val pw = new PrintWriter(file)
    pw.write("RecipeBook v0.1 data\n")
    pw.write("name : ingredients : main_unit : second_unit : density : tag : description : isMenu : amount\n")
    for (item <- food_list) {
      var food = item._1
      var num = item._2
      var boo = if (food.is_menu) "1" else "0"
      if (food.ingredients.isEmpty) {
        pw.write(food.name + "::" + food.main_unit + ":" + food.second_unit + ":" + food.density.toString + ":" + food.tag + ":" + food.description + ":" + boo + ":" + num.toString + "\n")
      } else {
        var ingredients_string = food.ingredients.toList.map(x => x._1.name + "=" + x._2.toString).mkString(",")
        pw.write(food.name + ":" + ingredients_string + ":" + food.main_unit + ":" + food.second_unit + ":" + food.density.toString + ":" + food.tag + ":" + food.description + ":" + boo + ":" + num.toString + "\n")
      }
    }
    pw.close
  }

  //IO Read Function
  def IORead() = {
    var lines = fromFile("src/saved_data/default.txt").getLines.filter(!_.isEmpty)
    var line_num = 1
    try {
      lines = fromFile("src/saved_data/data.txt").getLines.filter(!_.isEmpty)
      left_feedback.text = "> User-saved file loaded successfully. "
      left_feedback.repaint()
    } catch {
      case e: FileNotFoundException => {
        left_feedback.text = "> User-saved file not found. Loaded from default. "
        left_feedback.repaint()
      }
    }
    if (!lines.next().startsWith("RecipeBook")) {
      println("IOError")
    } else {
      try {
        lines.next()
        line_num = 2
        var buffer = ArrayBuffer[Holder]()
        for (line <- lines) {
          line_num += 1
          var splitted = line.split(":").map(_.trim())
          if (splitted.size != 9) throw new IOException
          var name_add: String = splitted(0)
          var ingredients_add: String = splitted(1)
          var first_unit_add: String = splitted(2).toLowerCase
          var second_unit_add: String = splitted(3).toLowerCase
          var density_add: Double = splitted(4).toDouble
          var alleriges_add: String = splitted(5).toUpperCase
          var description_add: String = splitted(6)
          var isMenu_add: Boolean = if (splitted(7) == "1") true else if (splitted(7) == "0") false else throw new IOException
          var amount_add: Double = splitted(8).toDouble
          if (amount_add > 1000) {
            amount_add = 1000
            println("Notice: The maximum amount allowed in this system is 1000. Your input has been changed to 1000.")
          }
          if (amount_add < 0) {
            amount_add = 0
            println("Notice: The amount cannot be negative. Your input has been changed to 0.")
          }
          if (density_add < 0) {
            density_add = 0
            println("Notice: Density cannot be negative. System changed it to the default value: 0")
          }
          if (ingredients_add.isEmpty) {
            var food_add = new Food(name_add, scala.collection.mutable.Map[Food, Double](), first_unit_add, second_unit_add, density_add, alleriges_add, description_add)
            if (isMenu_add) food_add.set_to_menu()
            fridge.add_food(food_add, amount_add)
          } else {
            var ingre_map: Map[String, Double] = {
              var item_list = ingredients_add.split(",")
              var name_list = ArrayBuffer[String]()
              var amount_list = ArrayBuffer[Double]()
              for (item <- item_list) {
                name_list += item.split("=").head.trim
                var temp_amount = item.split("=").last.toDouble
                if (temp_amount > 1000) {
                  temp_amount = 1000
                  println("Notice: The maximum amount allowed in this system is 1000. The amount of " + item.split("=").head.trim + "has been changed to 1000.")
                } else if (temp_amount < 0) {
                  temp_amount = 0
                  println("Notice: The amount cannot be negative. The amount of " + item.split("=").head.trim + "has been changed to 0.")
                }
                amount_list += temp_amount
              }
              (name_list zip amount_list).toMap
            }
            buffer += new Holder(name_add, ingre_map, first_unit_add, second_unit_add, density_add, alleriges_add, description_add, isMenu_add, amount_add)
          }
        }
        var accumulator = 0
        while (!buffer.isEmpty) {
          try {
            for (data <- buffer) {
              var pre_ingre_list = data.ingre.map(_._1)
              if (pre_ingre_list.forall(menu.return_food_with_name(_) != None)) {
                var ingre_mapped = data.ingre.map(x => (menu.return_food_with_name(x._1).get, x._2))
                var ingre = collection.mutable.Map(ingre_mapped.toSeq: _*)
                //var tag_ingre = (ingre.keys.map(_.tag).mkString("").toUpperCase+data.aller).distinct
                var food_add = new Food(data.name, ingre, data.first_u, data.second_u, data.density, data.aller, data.description)
                if (data.boo) food_add.set_to_menu()
                fridge.add_food(food_add, data.amount)
                buffer -= data
              }
            }
          } catch {
            case e: NullPointerException => {
              p("Notice: One food is waiting for raw material(s)")
            }
          }
          accumulator += 1
          if (accumulator > 100) {
            throw new IOException
          }
        }
        p("Notice: User Interface loaded successfully")
        p("Notice: " + menu.menu_foodlist.size.toString + " menus and " + menu.non_menu_foodlist.size.toString + " ingredients have been imported")
      } catch {
        case e: Exception => {
          left_feedback.text = "> Errors in file. Please check your input file. "
          left_feedback.foreground = RED
          button_save = button_exit
          search_button.enabled = false
          back_button.enabled = false
          left_multi_text.repaint()
          search_button.repaint()
          left_feedback.repaint()
          button_save.repaint()
        }
      }
    }
  }

  IORead()

  // Definitions
  def p[T](a: T) = if (settings.diagnosis) println(a.toString)

  def return_status() = (settings.all_abbri zip right_checkbox_list.map(_.selected))

  def update_allergies_string() = {
    settings.allergies_string = return_status().filter(_._2).map(_._1).mkString
  }
  def revalidate_window(box: BoxPanel) = {
    left_normal_menu_box.contents -= box
    if (changed == 1) change_box(search_box.text)
    left_normal_menu_box.repaint()
    left_normal_menu_box.revalidate()
    outer_box.repaint()
    outer_box.revalidate()
  }
  def refresh_menu_box() = {
    listenTo(search_box)
    while (!left_normal_menu_box.contents.isEmpty) {
      left_normal_menu_box.contents -= left_normal_menu_box.contents.last
    }
    var food_list_menu = food_list.filter(_._1.is_menu()).toSeq.sortBy(x => menu.check_availability(x._1)).reverse.toMap
    var allergies = (settings.all_abbri zip right_checkbox_list.map(_.selected)).filter(_._2).map(_._1)
    if (allergies.isEmpty) allergies = List[String]()
    var food_list_menu_allergies = food_list_menu.filter(x => allergies.forall(y => x._1.tag.contains(y)))
    for (item <- food_list_menu_allergies) {
      var item_food = item._1
      var item_amount = item._2
      left_normal_menu_box.contents += new UISectionBox(item_food, this).default_box
    }
    outer_box.repaint()
    outer_box.revalidate()
  }
  def change_box(keyword: String) = {
    var sub_ui = new UISearchRepresentation(this, keyword)
    while (!left_normal_menu_box.contents.isEmpty) {
      left_normal_menu_box.contents -= left_normal_menu_box.contents.last
    }
    left_normal_menu_box.contents += sub_ui.headline_border
    left_normal_menu_box.contents += VStrut(40)
    left_normal_menu_box.contents += sub_ui.box1_border
    left_normal_menu_box.contents += VStrut(20)
    left_normal_menu_box.contents += sub_ui.box2_border
    left_normal_menu_box.contents += VStrut(20)
    if (!sub_ui.keyDouble.isNaN()) left_normal_menu_box.contents += sub_ui.box3_border
    listenTo(search_box)
    left_normal_menu_box.repaint()
    left_menu_scroll.revalidate()
    outer_box.repaint()
    outer_box.revalidate()
  }
  def add_menu_to_ui(str: String) = {
    try {
      var str_list = str.split(":").map(_.trim())
      p("Input string: " + str_list.mkString(":"))
      if (str_list.size != 9) throw new Exception
      var name_add: String = str_list(0)
      var ingredients_add: String = str_list(1)
      var first_unit_add: String = str_list(2)toLowerCase
      var second_unit_add: String = str_list(3).toLowerCase
      var density_add: Double = str_list(4).toDouble
      var alleriges_add: String = str_list(5).toUpperCase
      var description_add: String = str_list(6)
      var isMenu_add: Boolean = if (str_list(7) == "1") true else if (str_list(7) == "0") false else throw new Exception
      var amount_add: Double = str_list(8).toDouble
      if ((edit == 0) && (menu.return_food_with_name(name_add) != None)) throw new IOException
      if (amount_add > 1000) {
        amount_add = 1000
        println("Notice: The maximum amount allowed in this system is 1000. Your input has been changed to 1000.")
      }
      if (amount_add < 0) {
        amount_add = 0
        println("Notice: The amount cannot be negative. Your input has been changed to 0.")
      }
      if (density_add < 0) {
        density_add = 0
        println("Notice: Density cannot be negative. System changed it to the default value: 0")
      }
      if (ingredients_add.isEmpty) {
        var food_add = new Food(name_add, scala.collection.mutable.Map[Food, Double](), first_unit_add, second_unit_add, density_add, alleriges_add, description_add)
        if (isMenu_add) food_add.set_to_menu()
        menu.fridge.add_food(food_add, amount_add)
      } else {
        var ingre_map: collection.mutable.Map[Food, Double] = {
          var item_list = ingredients_add.split(",")
          var name_list = ArrayBuffer[Food]()
          var amount_list = ArrayBuffer[Double]()
          for (item <- item_list) {
            name_list += menu.return_food_with_name(item.split("=").head.trim).get
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
        if (isMenu_add) food_add.set_to_menu()
        menu.fridge.add_food(food_add, amount_add)
      }
      if (edit == 1) menu.fridge.food_list -= editing
      left_feedback.text = "> Added/Modified successfully!"
    } catch {
      case e: IOException => {
        left_feedback.text = "> Failed. The menu with the same name already exists"
      }
      case e: NoSuchElementException => {
        left_feedback.text = "> Failed. One or more ingredients is missing. Please add ingredients first"
      }
      case e: Exception => {
        left_feedback.text = "> Failed. Wrong format"
      }
    } finally {
      left_feedback.repaint()
    }
    search_box.text = if (temp_search_text.isEmpty()) " Search for recipes or ingredients here..." else temp_search_text
    search_box.foreground = GRAY
  }

  // Icons
  var icon_selected = new ImageIcon("src/icons/selected.png")
  var icon_free = new ImageIcon("src/icons/free.png")
  var icon_button = new ImageIcon("src/icons/button.png")
  var icon_save = new ImageIcon("src/icons/save.png")
  var icon_save_pressed = new ImageIcon("src/icons/save_done.png")
  var icon_exit = new ImageIcon("src/icons/exit.png")
  var icon_find = new ImageIcon("src/icons/find.png")
  var icon_back = new ImageIcon("src/icons/back.png")
  var icon_tick = new ImageIcon("src/icons/tick.png")

  // Left Welcome Label
  left_welcome.horizontalAlignment = Left
  left_welcome.font = new Font("Arial", 0, 80)
  left_info_section.contents += VStrut(20)
  left_info_section.contents += left_welcome

  // Left Info Box
  left_info_section.background = WHITE
  left_info_section.border = EmptyBorder(30, 30, 30, 30)

  // Left Menu Box ScrollPane Frame
  left_menu_scroll.preferredSize = new Dimension(1440, 600)
  left_info_section.contents += VStrut(20)
  left_info_section.contents += left_menu_scroll

  // Left Menu BoxPanel Normal
  refresh_menu_box()
  left_menu_scroll.contents = left_normal_menu_box

  // Left Search Area
  left_search_area.preferredSize = new Dimension(1440, 100)
  left_search_area.background = WHITE
  left_info_section.contents += VStrut(10)

  // Left Search Prevention TextField (Avoiding cursor move to search box after clicking "MAKE")
  search_prevention_box.font = new Font("Arial", 0, 1)
  search_prevention_box.border = BorderFactory.createEmptyBorder()
  left_info_section.contents += search_prevention_box

  // Left Search TextField
  search_box.font = new Font("Arial", 0, 50)
  search_box.foreground = GRAY
  search_box.border = BorderFactory.createLineBorder(my_color, 5)
  listenTo(search_box)
  reactions += {
    case e: FocusGained => {
      p("Notice: Search box gained focus")
      search_box.text = ""
      search_box.foreground = BLACK
      outer_box.repaint()
    }
  }
  search_button.background = WHITE
  search_button.font = new Font("Arial", 0, 50)
  search_button.preferredSize = new Dimension(100, 100)
  search_button.border = BorderFactory.createLineBorder(my_color, 5)
  search_button.icon = icon_find
  back_button.background = WHITE
  back_button.font = new Font("Arial", 0, 50)
  back_button.preferredSize = new Dimension(100, 100)
  back_button.border = BorderFactory.createLineBorder(my_color, 5)
  back_button.icon = icon_back
  left_search_area.contents += search_box
  left_search_area.contents += search_button
  left_search_area.contents += back_button
  left_info_section.contents += left_search_area
  left_info_section.contents += VStrut(20)

  // Left Real-time feedback TextField
  left_feedback.preferredSize = new Dimension(200, 40)
  left_feedback.font = new Font("Arial", 0, 34)
  left_feedback.border = BorderFactory.createEmptyBorder()
  left_feedback.editable = false
  left_feedback.background = WHITE
  left_info_section.contents += left_feedback
  left_info_section.contents += VStrut(20)

  // Left Multi-usage Testfield
  left_multi_text.editable = false
  left_multi_text.background = WHITE
  left_multi_text.preferredSize = new Dimension(1300, 30)
  left_multi_text.font = new Font("Arial", 0, 30)
  left_multi_text.border = BorderFactory.createEmptyBorder()

  // Left Muti-usage Button
  left_multi_button.font = new Font("Arial", 0, 30)
  left_multi_button.border = BorderFactory.createEmptyBorder()
  left_multi_button.preferredSize = new Dimension(50, 50)
  left_multi_button.background = WHITE
  left_multi_button.visible = false
  left_multi_button.icon = icon_tick
  listenTo(left_multi_button)
  reactions += {
    case e: ButtonClicked => {
      p("Notice: Complete button pressed")
      left_multi_button.visible = false
      right_checkbox_list.foreach(_.visible = true)
      button_save.visible = true
      left_multi_button.repaint()
      left_multi_button.revalidate()
      outer_box.repaint()
      outer_box.revalidate()
    }
  }

  // Left multi-usage box
  left_multi_box.background = WHITE
  left_multi_box.contents += left_multi_text
  left_multi_box.contents += HStrut(10)
  left_multi_box.contents += left_multi_button

  // Left multi-usage frame
  left_multi_frame.preferredSize = new Dimension(1440, 50)
  left_multi_frame.background = WHITE
  left_multi_frame.layout(left_multi_box) = West
  left_info_section.contents += left_multi_frame

  // Right Welcome Label
  right_welcome.horizontalAlignment = Left
  right_welcome.font = new Font("Arial", 0, 64)
  right_welcome.foreground = WHITE
  right_welcome.opaque = false

  // Right Checkboxes
  var i = 0
  while (i < settings.allergies.size) {
    var current = new CheckBox(settings.allergies(i))
    current.opaque = false
    current.foreground = WHITE
    current.font = new Font("Arial", 0, 50)
    current.selectedIcon = icon_selected
    current.icon = icon_free
    current.iconTextGap = 10
    right_checkbox_list += current
    i += 1
  }
  right_checkbox_list.map(listenTo(_))
  reactions += {
    case e: ButtonClicked => {
      var allergies = (settings.all_abbri zip right_checkbox_list.map(_.selected)).filter(_._2).map(_._1)
      p("Notice: Checkbox(es) selection changed, new allergen list is: " + allergies.mkString(""))
      if (changed == 0) {
        search_box.text = " Search for recipes or ingredients here..."
        search_box.foreground = GRAY
        refresh_menu_box()
      } else {
        search_box.text = temp_search_text
        change_box(search_box.text)
      }
      left_multi_box.repaint()
      left_multi_box.revalidate()
      outer_box.repaint()
      outer_box.revalidate()
    }
  }

  // Right Save Button
  button_save.icon = icon_save
  button_save.background = WHITE
  button_save.opaque = false
  button_save.border = BorderFactory.createEmptyBorder()
  button_save.icon = icon_save
  button_save.pressedIcon = icon_save_pressed

  // Right Exit Button (Shows only with IOError)
  button_exit.icon = icon_exit
  button_exit.pressedIcon = icon_exit
  button_exit.background = WHITE
  button_exit.opaque = false
  button_exit.border = BorderFactory.createEmptyBorder()
  button_exit.icon = icon_exit

  // Right Info Box
  right_info_section.contents += right_welcome
  right_info_section.contents += VStrut(30)
  for (checkbox <- right_checkbox_list) {
    right_info_section.contents += checkbox
    right_info_section.contents += VStrut(20)
  }
  right_info_section.contents += VStrut(200)
  right_info_section.contents += button_save
  right_info_section.background = my_color
  right_info_section.border = EmptyBorder(20, 20, 20, 20)

  // Frame Section
  outer_box.contents += left_box
  outer_box.contents += right_box

  // Left Panel Section
  left_box.preferredSize = new Dimension(1440, 1080)
  left_box.layout(left_info_section) = North
  left_box.background = WHITE
  contents = outer_box

  // Right Panel Section
  right_box.preferredSize = new Dimension(480, 1080)
  right_box.layout(right_info_section) = North
  right_box.background = my_color

}

object UI extends App {
  def main() {
    val ui = new UI
    ui.visible = true
  }
  private def run() = {
    main()
  }
  this.run()
}