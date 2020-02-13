package cook
import scala.collection.mutable.ArrayBuffer
import scala.io.Source._
import scala.util.control.Breaks._
import java.awt.Color._
import java.io._
import javax.swing.ImageIcon
import javax.swing.BorderFactory

class FileReader(ui: UI) {
  /**IO Write Function*/
  def IOWrite() = {
    var file = new File("src/saved_data/data.txt")
    val pw = new PrintWriter(file)
    pw.write("RecipeBook v0.1 data\n")
    pw.write("name : ingredients : main_unit : second_unit : density : tag : description : isMenu : amount\n")
    for (item <- ui.food_list) {
      var food = item._1
      var num = item._2
      var boo = if (food.is_menu) "1" else "0"
      var ingredients_string = ""
      if (food.ingredients.nonEmpty) {
        var ingredients_string = food.ingredients.toList.map(x => x._1.name + "=" + x._2.toString).mkString(",")
      }
      pw.write(food.name + ":" + ingredients_string + ":" + food.main_unit + ":" + food.second_unit + ":" + food.density.toString + ":" + food.tag + ":" + food.description + ":" + boo + ":" + num.toString + "\n")
    }
    pw.close
  }

  /**IO Read Function*/
  def IORead() = {
    var lines = fromFile("src/saved_data/default.txt").getLines().filter(_.nonEmpty)
    try {
      lines = fromFile("src/saved_data/data.txt").getLines().filter(_.nonEmpty)
      ui.left_feedback.text = "> User saved file loaded successfully. "
    } catch {
      case e: FileNotFoundException => {
        ui.left_feedback.text = "> No user saved file found. Loaded from default. "
      }
    } finally {
      ui.left_feedback.repaint()
    }
    if (!lines.next().startsWith("RecipeBook")) {
      println("IOError")
    } else {
      try {
        lines.next()
        var buffer = ArrayBuffer[ArrayBuffer[Any]]()
        for (line <- lines) {
          var splitted = line.split(":").map(_.trim)
          if (splitted.size != 9) throw new IOException
          var name_add: String = splitted(0)
          var ingredients_add: String = splitted(1)
          var first_unit_add: String = splitted(2).toLowerCase
          var second_unit_add: String = splitted(3)toLowerCase
          var density_add: Double = splitted(4).toDouble
          var alleriges_add: String = splitted(5).toUpperCase
          var description_add: String = splitted(6)
          var isMenu_add: Boolean = if (splitted(7) == "1") true else if (splitted(7) == "0") false else throw new IOException
          var amount_add: Double = splitted(8).toDouble
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
            if (isMenu_add) food_add.set_to_menu()
            ui.fridge.add_food(food_add, amount_add)
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
            buffer += ArrayBuffer[Any](name_add, ingre_map, first_unit_add, second_unit_add, density_add, alleriges_add, description_add, isMenu_add, amount_add)
          }
        }
        var accumulator = 0
        while (buffer.nonEmpty) {
          try {
            for (data <- buffer) {
              var name_any = data(0).asInstanceOf[String]
              var map_any = data(1).asInstanceOf[Map[String, Double]]
              var first_unit_any: String = data(2).asInstanceOf[String]
              var second_unit_any: String = data(3).asInstanceOf[String]
              var density_any: Double = data(4).asInstanceOf[Double]
              var alleriges_any: String = data(5).asInstanceOf[String]
              var description_any: String = data(6).asInstanceOf[String]
              var isMenu_any: Boolean = data(7).asInstanceOf[Boolean]
              var amount_any: Double = data(8).asInstanceOf[Double]
              if (map_any.map(_._1).forall(ui.menu.return_food_with_name(_) != None)) {
                var ingre_mapped = map_any.map(x => (ui.menu.return_food_with_name(x._1).get, x._2))
                var ingre = collection.mutable.Map(ingre_mapped.toSeq: _*)
                //var tag_ingre = (ingre.keys.map(_.tag).mkString("").toUpperCase+data.aller).distinct
                var food_add = new Food(name_any, ingre, first_unit_any, second_unit_any, density_any, alleriges_any, description_any)
                if (isMenu_any) food_add.set_to_menu()
                ui.fridge.add_food(food_add, amount_any)
                buffer -= data
              }
            }
          } catch {
            case e: NullPointerException => {
              ui.p("Notice: One food is waiting for raw material(s)")
            }
          }
          accumulator += 1
          if (accumulator > 100) throw new IOException
        }
        ui.p("Notice: User Interface loaded successfully")
        ui.p("Notice: " + ui.menu.menu_foodlist.size.toString + " menus and " + ui.menu.non_menu_foodlist.size.toString + " ingredients have been imported")
      } catch {
        case e: Exception => {
          ui.left_feedback.text = "> Errors in file. Please check your input file. "
          ui.left_feedback.foreground = RED
          ui.button_save = ui.button_exit
          ui.search_button.enabled = false
          ui.back_button.enabled = false
          ui.left_multi_text.repaint()
          ui.search_button.repaint()
          ui.left_feedback.repaint()
          ui.button_save.repaint()
        }
      }
    }
  }
}