package cook
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParHashMap
import scala.io.Source.fromFile
import scala.util.Using
import java.io.{File, PrintWriter, FileNotFoundException}
import java.awt.Color.RED

case class Holder(
    val name: String,
    val ingredients: Map[String, Double],
    val allergies: Set[Char],
    val description: String,
    val isMenu: Boolean,
    val amount: Double
)

class FileProcessor(ui: UI) {

  private val menu: FoodMenu = ui.menu

  def IOWritelines(): Unit = {
    val file = File("src/main/scala/saved_data/data.tsv")
    val pw = PrintWriter(file)
    pw.write("name\tingredients\ttag\tdescription\tisMenu\tamount\n")
    for ((food, num) <- menu.foodMap) {
      var ingredientsString = ""
      if (!food.hasNoIngredients) {
        ingredientsString = food.ingredients.toList
          .map(x => x._1.name + "=" + x._2.toString)
          .mkString("\t")
      }
      pw.write(
        food.name + "\t" + ingredientsString + "\t" + food.tag.mkString + "\t" + food.description + "\t" + food.isMenu + "\t" + num.toString + "\n"
      )
    }
    pw.close()
  }

  def IOReadlines(): Array[String] = {
    var lines = Array[String]()
    Using(fromFile("src/main/scala/saved_data/default.tsv")) { source =>
      lines = source.getLines().filter(_.nonEmpty).toArray
    }

    try {
      Using(fromFile("src/main/scala/saved_data/data.tsv")) { source =>
        lines = source.getLines().filter(_.nonEmpty).toArray
      }
      ui.leftFeedback.text = "> User-saved file loaded successfully. "
    } catch {
      case _: FileNotFoundException =>
        ui.leftFeedback.text =
          "> User-saved file not found. Loaded from default. "
    } finally {
      ui.leftFeedback.repaint()
    }

    lines.tail
  }

  def lineParser(line: String): Option[Holder] = {
    val lineArray = line.split("\t").map(_.trim)
    if (lineArray.length != 6) {
      throw Exception("Invalid line length")
    }
    val tooManyLinesMessage =
      "The maximum amount allowed in this system is 1000. Your input has been changed to 1000."
    val negativeAmountMessage =
      "The amount cannot be negative. Your input has been changed to 0."
    try {
      val name = lineArray(0)
      val ingredients = if (lineArray(1).nonEmpty) {
        lineArray(1)
          .split(",")
          .map(x => {
            val ingredient = x.split("=")
            val ingredientName = ingredient(0)
            var ingredientAmount = ingredient(1).toDouble
            if (ingredientAmount > 1000) {
              ingredientAmount = 1000
              println("Notice: " + tooManyLinesMessage)
            } else if (ingredientAmount < 0) {
              ingredientAmount = 0
              println("Notice: " + negativeAmountMessage)
            }
            ingredientName -> ingredientAmount
          })
          .toMap
      } else { Map[String, Double]() }
      val tag = lineArray(2).toCharArray.toSet
      val description = lineArray(3)
      val isMenu = lineArray(4).toBoolean
      var amount = lineArray(5).toDouble
      if (amount > 1000) {
        amount = 1000
        println("Notice: " + tooManyLinesMessage)
      } else if (amount < 0) {
        amount = 0
        println("Notice: " + negativeAmountMessage)
      }
      Some(Holder(name, ingredients, tag, description, isMenu, amount))
    } catch {
      case _: Exception => {
        println("Invalid line: " + line)
        None
      }
    }
  }

  def linesToUI(lines: Array[String] = IOReadlines()) = {
    var holders = lines.map(lineParser).filter(_.isDefined).map(_.get)
    var continue = true
    while (continue) {
      val toBeAdded = ArrayBuffer[Holder]()
      for (holder <- holders) {
        if (holder.ingredients.isEmpty) {
          val food = Food(
            holder.name,
            ParHashMap[Food, Double](),
            holder.allergies,
            holder.description
          )
          if (holder.isMenu) food.setToMenu()
          menu.addFood(food, holder.amount)
        } else {
          val allIngredientsFound = holder.ingredients.forall(x =>
            menu.getFoodArray.exists(_.name == x._1)
          )
          if (allIngredientsFound) {
            var ingredients = ParHashMap[Food, Double]()
            for (ingredient <- holder.ingredients) {
              val food =
                menu.getFoodArray.find(x => x.name == ingredient._1).get
              ingredients += (food -> ingredient._2)
            }
            val food = Food(
              holder.name,
              ingredients,
              holder.allergies,
              holder.description
            )
            if (holder.isMenu) food.setToMenu()
            menu.addFood(food, holder.amount)
          } else {
            toBeAdded += holder
          }
        }
      }

      if (toBeAdded.length == holders.length) {
        continue = false
      } else {
        holders = toBeAdded.toArray
        toBeAdded.clear()
      }
    }
  }
}
