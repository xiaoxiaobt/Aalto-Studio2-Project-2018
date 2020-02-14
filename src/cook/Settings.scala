package cook
import java.awt.Color
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

object Settings {
  // System Settings
  val diagnosis = true
  // Allergies Settings (default)
  val allergies = List[String]("With Allergens", "Gluten free", "Lactose free", "Milk free", "Vegan", "Vegeterian")
  val all_abbri = List[String]("A", "G", "L", "M", "V", "W")
  var allergies_string = ""
  //  Color
  val color = new Color(120, 200, 220)
  // Scaling factor
  var scale: Double = 0.5
  def scaleTo(num: Int):Int = (scale * num).toInt
}