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
  var default_box = new BoxPanel(Vertical)
  var first_row = new BoxPanel(Horizontal)
  var label_name = new Label(" " + food.name + " " * (28 - food.name.length))
  var first_row_iconset = new BoxPanel(Horizontal)
  var icon_boxes = ArrayBuffer.fill[Button](6)(Button("") {})
  var button_del = Button(" x ") {
    menu.fridge.food_list -= food
    p("Notice: " + food.name + " has been removed from the list")
    ui.revalidate_window(default_box)
  }
  var button_add = Button(" + ") {
    ui.right_checkbox_list.foreach(_.visible = false)
    ui.button_save.visible = false
    ui.left_multi_button.visible = true
    ui.edit = false
    ui.deafTo(ui.search_box)
    if (!ui.changed) ui.refresh_menu_box() else ui.change_box(ui.search_box.text)
    ui.left_menu_scroll.revalidate()
    var edit_string = "Example_name:Ingredient_a=1,Ingredient_b=2:unit_1:unit_2:0:Allengens:Description:isMenudigit (1=yes, 0=no):amount"
    p("Adding string: " + edit_string)
    ui.left_multi_text.editable = true
    ui.left_multi_text.text = edit_string
    ui.left_multi_text.border = BorderFactory.createLineBorder(my_color, scaleTo(5))
    ui.left_feedback.text = "> Edit menu in given format (Example below), press green Complete button when finished"
  }
  var button_mod = Button("") {
    ui.right_checkbox_list.foreach(_.visible = false)
    ui.button_save.visible = false
    ui.right_box.revalidate()
    ui.left_multi_button.visible = true
    ui.edit = true
    ui.editing = food
    ui.deafTo(ui.search_box)
    var boo = if (food.is_menu) "1" else "0"
    var ingredients_string = {
      if (food.ingredients.isEmpty) {
        ""
      } else {
        food.ingredients.toList.map(x => x._1.name + "=" + x._2.toString).mkString(",")
      }
    }
    var edit_string = food.name + ":" + ingredients_string + ":" + food.main_unit + ":" + food.second_unit + ":" + food.density.toString + ":" + food.tag + ":" + food.description + ":" + boo + ":" + menu.fridge.food_list(food).toString
    p("Editing string: " + edit_string)
    ui.left_multi_text.text = edit_string
    ui.left_multi_text.editable = true
    ui.left_multi_text.border = BorderFactory.createLineBorder(my_color, scaleTo(5))
    ui.left_feedback.text = "> Edit menu in given format in the box below, press green Complete button when finished"
  }
  var edit_icon = new ImageIcon("src/icons/edit.png")
  button_mod.icon = edit_icon
  var label_des = new Label("   Description: " + food.description)
  def d2i(num: Double) = if (num.toInt.toDouble == num) num.toInt.toString else num.toString
  var label_ingre = new Label("   Ingredients: " + food.ingredients.toList.map(x => x._1.name + "×" + d2i(x._2) + x._1.main_unit).mkString(", "))
  if (food.ingredients.isEmpty) label_ingre.text = "   " + food.name + " is an ingredient. "
  var first_part = new BorderPanel
  var second_part = new BorderPanel
  var third_part = new BorderPanel
  var last_part = new BorderPanel
  var last_row = new BoxPanel(Horizontal)
  var label_ready = new Label("Ready to eat: " + menu.fridge.food_list(food).toInt.toString)
  if (food.ingredients.isEmpty) label_ready.text = "Amount: " + menu.fridge.food_list(food).toInt.toString
  var label_cookable = new Label("Cookable: " + (menu.check_availability(food) - menu.fridge.food_list(food).toInt).toString)
  if (food.ingredients.isEmpty) label_cookable.visible = false
  var button_make = Button("  Use/Make  ") {
    menu.make(food, 1)
    p("Notice: 1 " + food.name + " has been made/consumed")
    if (!ui.changed) ui.refresh_menu_box() else ui.change_box(ui.search_box.text)
    ui.left_normal_menu_box.contents -= default_box
    ui.outer_box.revalidate()
  }
  if (food.ingredients.isEmpty) button_make.text = "       Use       "

  //first_row
  label_name.font = new Font("Consolas", 0, scaleTo(48))
  label_name.foreground = my_color
  label_name.horizontalAlignment = Left
  label_name.preferredSize = new Dimension(scaleTo(1330), scaleTo(60))
  for (x <- icon_boxes) {
    first_row_iconset.contents += x
    x.border = BorderFactory.createEmptyBorder()
    x.background = WHITE
    x.preferredSize = new Dimension(scaleTo(30), scaleTo(30))
  }
  button_add.font = new Font("Arial", 0, scaleTo(40))
  button_add.border = BorderFactory.createEmptyBorder()
  button_add.opaque = false
  button_add.background = WHITE
  button_add.foreground = GREEN
  button_mod.font = new Font("Arial", 0, scaleTo(40))
  button_mod.border = BorderFactory.createEmptyBorder()
  button_mod.opaque = false
  button_mod.background = WHITE
  button_mod.foreground = BLUE
  button_del.font = new Font("Arial", 0, scaleTo(40))
  button_del.border = BorderFactory.createEmptyBorder()
  button_del.opaque = false
  button_del.background = WHITE
  button_del.foreground = RED
  first_row.contents += label_name
  first_row.contents += first_row_iconset
  first_row.contents += HStrut(scaleTo(280))
  first_row.contents += button_add
  first_row.contents += button_mod
  first_row.contents += button_del

  var foodTag = food.tag.toUpperCase
  var tagPair = "AGLMVW".zipWithIndex
  // Icon A/G/L/M/V/W
  for ((letter, index) <- tagPair) {
    if (foodTag.contains(letter)) {
      icon_boxes(index).icon = new ImageIcon("src/icons/B_" + letter + ".png")
    } else {
      icon_boxes(index).icon = new ImageIcon("src/icons/W_" + letter + ".png")
    }
  }

  // Second row: Description
  label_des.font = new Font("Arial", 0, scaleTo(36))
  // Third row: Ingredients
  label_ingre.font = new Font("Arial", 0, scaleTo(36))
  // Last row: Cooked, Cookable & Make
  label_ready.font = new Font("Arial", 0, scaleTo(30))
  label_cookable.font = new Font("Arial", 0, scaleTo(30))
  if (menu.fridge.food_list(food) > 0) {
    label_ready.foreground = ORANGE
  } else {
    label_ready.visible = false
  }
  if (menu.check_availability(food) > 0) {
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

  first_part.layout(first_row) = West
  second_part.layout(label_des) = West
  third_part.layout(label_ingre) = West
  last_part.layout(last_row) = East

  default_box.contents += first_part
  default_box.contents += second_part
  default_box.contents += third_part
  default_box.contents += last_part
  default_box.preferredSize = new Dimension(scaleTo(1330), scaleTo(200))
  default_box.border = BorderFactory.createLineBorder(my_color, scaleTo(1))

  def add_menu_from_ui(str: String, remove: Boolean) = {
    try {
      var str_list = str.split(":").map(_.trim())
      p(str_list.mkString(":"))
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

      if (menu.return_food_with_name(name_add) != None) throw new IOException
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
            name_list += menu.return_food_with_name(item.split("-").head).get
            amount_list += item.split("-").last.toDouble
          }
          var temp = (name_list zip amount_list).toMap
          collection.mutable.Map(temp.toSeq: _*)
        }
        var food_add = new Food(name_add, ingre_map, first_unit_add, second_unit_add, density_add, alleriges_add, description_add)
        if (isMenu_add) food_add.set_to_menu()
        menu.fridge.add_food(food_add, amount_add)
      }
      ui.left_feedback.text = "> Added successfully!"
      ui.outer_box.repaint()
      ui.outer_box.revalidate()
    } catch {
      case e: IOException => {
        ui.left_feedback.text = "> Failed. The menu with the same name already exists"
      }
      case e: Exception => {
        ui.left_feedback.text = "> Failed. Error inputs."
      }
    }
    ui.left_feedback.repaint()
    if (ui.temp_search_text.isEmpty) {
      ui.search_box.text = " Search for recipes or ingredients here..."
    } else {
      ui.search_box.text = ui.temp_search_text
    }
    ui.search_box.foreground = GRAY
    if (!ui.changed) ui.refresh_menu_box() else ui.change_box(ui.search_box.text)
  }

}