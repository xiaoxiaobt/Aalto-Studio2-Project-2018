package cook
import java.awt.Color
import scala.collection.mutable.Map

object Settings {
  // System Settings
  val diagnosis = true
  // Allergies Settings (default)
  val allergies: List[String] = List(
    "With Allergens",
    "Gluten free",
    "Lactose free",
    "Diary free",
    "Vegan",
    "Vegetarian"
  )
  val allAbbreviations: Set[Char] = Set('A', 'G', 'L', 'M', 'V', 'W')
  // Color
  val color: Color = Color(120, 200, 220)
  // Scaling factor
  // val scale: Double = 0.5
  // def scaleTo(num: Int): Int = (scale * num).toInt
}
