package cook
import scala.collection.mutable.ArrayBuffer
import scala.io.Source._
import java.io._
import java.awt.Color.RED

class FileProcessor(ui: UI) {
  var menu: Menu = ui.menu
  /**IO Write Function*/
  def IOWrite() = {
    val file = new File("src/main/scala/saved_data/data.txt")
    val pw = new PrintWriter(file)
    pw.write("RecipeBook v0.1 data\n")
    pw.write("name \t ingredients \t main_unit \t second_unit \t density \t tag \t description \t isMenu \t amount\n")
    for ((food, num) <- menu.foodList) {
      val boo = if (food.isMenu) "1" else "0"
      var ingredientsString = ""
      if (!food.hasNoIngredients) {
        ingredientsString = food.ingredients.toList.map(x => x._1.name + "=" + x._2.toString).mkString(",")
      }
      pw.write(food.name + "\t" + ingredientsString + "\t" + food.main_unit + "\t" + food.second_unit + "\t" + food.density.toString + "\t" + food.tag + "\t" + food.description + "\t" + boo + "\t" + num.toString + "\n")
    }
    pw.close()
  }

  /**IO Read Function*/
  def IOReadlines(): Iterator[String] = {
    var lines = fromFile("src/main/scala/saved_data/default.txt").getLines.filter(_.nonEmpty)
    try {
      lines = fromFile("src/main/scala/saved_data/data.txt").getLines.filter(_.nonEmpty)
      ui.leftFeedback.text = "> User-saved file loaded successfully. "
    } catch {
      case _: FileNotFoundException => ui.leftFeedback.text = "> User-saved file not found. Loaded from default. "
    } finally {
      ui.leftFeedback.repaint()
      if (!lines.next().startsWith("RecipeBook")) throw new IOException
      lines.next() // Read header
    }
    lines
  }

  def lineProcessor(line: String, container: ArrayBuffer[Array[Any]]): Unit = {
    val splitted = line.split("\t").map(_.trim)
    if (splitted.size != 9) throw new IOException
    val nameAdd: String = splitted(0)
    val ingredients_add: String = splitted(1)
    val firstUnitAdd: String = splitted(2).toLowerCase
    val secondUnitAdd: String = splitted(3).toLowerCase
    var densityAdd: Double = splitted(4).toDouble
    val allergiesAdd: String = splitted(5).toUpperCase
    val descriptionAdd: String = splitted(6)
    val isMenuAdd: Boolean = if (splitted(7) == "1") true else if (splitted(7) == "0") false else throw new IOException
    var amountAdd: Double = splitted(8).toDouble
    if (amountAdd > 1000) {
      amountAdd = 1000
      println("Notice: The maximum amount allowed in this system is 1000. Your input has been changed to 1000.")
    } else if (amountAdd < 0) {
      amountAdd = 0
      println("Notice: The amount cannot be negative. Your input has been changed to 0.")
    }
    if (densityAdd < 0) {
      densityAdd = 0
      println("Notice: Density cannot be negative. System changed it to the default value: 0")
    }
    if (ingredients_add.isEmpty) {
      val foodAdd = Food(nameAdd, scala.collection.mutable.Map[Food, Double](), firstUnitAdd, secondUnitAdd, densityAdd, allergiesAdd, descriptionAdd)
      if (isMenuAdd) foodAdd.setToMenu()
      menu.addFood(foodAdd, amountAdd)
    } else {
      val ingreMap: Map[String, Double] = {
        val itemList = ingredients_add.split(",")
        var nameList = ArrayBuffer[String]()
        var amountList = ArrayBuffer[Double]()
        for (item <- itemList) {
          nameList += item.split("=").head.trim
          var tempAmount = item.split("=").last.toDouble
          if (tempAmount > 1000) {
            tempAmount = 1000
            println("Notice: The maximum amount allowed in this system is 1000. The amount of " + item.split("=").head.trim + "has been changed to 1000.")
          } else if (tempAmount < 0) {
            tempAmount = 0
            println("Notice: The amount cannot be negative. The amount of " + item.split("=").head.trim + "has been changed to 0.")
          }
          amountList += tempAmount
        }
        (nameList zip amountList).toMap
      }
      container += Array[Any](nameAdd, ingreMap, firstUnitAdd, secondUnitAdd, densityAdd, allergiesAdd, descriptionAdd, isMenuAdd, amountAdd)
    }
  }

  def loadFromIO() = {
    val lines = IOReadlines()
    try {
      var buffer = ArrayBuffer[Array[Any]]()
      for (line <- lines) lineProcessor(line, buffer)
      var accumulator = 0
      val threshold = 100
      while (buffer.nonEmpty) {
        try {
          for (data <- buffer) {
            val name = data(0).asInstanceOf[String]
            val ingredients = data(1).asInstanceOf[Map[String, Double]]
            val firstUnit = data(2).asInstanceOf[String]
            val secondUnit = data(3).asInstanceOf[String]
            val density = data(4).asInstanceOf[Double]
            val allergies = data(5).asInstanceOf[String]
            val description = data(6).asInstanceOf[String]
            val isMenu = data(7).asInstanceOf[Boolean]
            val amount = data(8).asInstanceOf[Double]
            val ingredientNames = ingredients.keys
            if (menu.allIngredientsExist(ingredientNames)) {
              val ingreMapped = ingredients.map(x => (menu.returnFoodWithName(x._1).get, x._2))
              val ingre = collection.mutable.Map(ingreMapped.toSeq: _*)
              //var tag_ingre = (ingre.keys.map(_.tag).mkString("").toUpperCase+data.aller).distinct
              val foodToBeAdd = Food(name, ingre, firstUnit, secondUnit, density, allergies, description)
              if (isMenu) foodToBeAdd.setToMenu()
              menu.addFood(foodToBeAdd, amount)
              buffer -= data
            }
          }
        } catch {
          case _: NullPointerException => ui.p("Notice: One food is waiting for raw material(s)")
        }
        accumulator += 1
        if (accumulator > threshold) throw new IOException
      }
      ui.p("Notice: User Interface loaded successfully")
      ui.p("Notice: " + menu.menuFoodlist.size.toString + " menus and " + menu.nonMenuFoodlist.size.toString + " ingredients have been imported")
    } catch {
      case _: IOException =>
        ui.leftFeedback.text = "> Errors in file. Please check your input file. "
        ui.leftFeedback.foreground = RED
        ui.buttonSave = ui.buttonExit
        ui.searchButton.enabled = false
        ui.backButton.enabled = false
        ui.leftMultifunctionalText.repaint()
        ui.searchButton.repaint()
        ui.leftFeedback.repaint()
        ui.buttonSave.repaint()
    }
  }

}