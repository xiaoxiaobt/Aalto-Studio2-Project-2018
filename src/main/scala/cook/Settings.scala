package cook
import java.awt.Color
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

object Settings {
  // System Settings
  val diagnosis = true
  // Allergies Settings (default)
  val allergies: List[String] = List("With Allergens", "Gluten free", "Lactose free", "Milk free", "Vegan", "Vegetarian")
  val allAbbreviations: List[String] = List("A", "G", "L", "M", "V", "W")
  var allergiesString = ""
  // Color
  val color = new Color(120, 200, 220)
  // Scaling factor
  var scale: Double = 0.5
  def scaleTo(num: Int): Int = (scale * num).toInt
}