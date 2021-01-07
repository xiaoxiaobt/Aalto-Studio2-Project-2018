import cook._
import org.scalatest._
import matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.Map

class UnitTests extends AnyFlatSpec with Matchers {

  /** Unit test 1 */
  "Fridge add/remove functions" should "work correctly" in {
    val testMenu = new FoodMenu()
    testMenu.menu should have size (0)
    val food1 = Food("Cookies", Map[Food, Double](), "pcs", "g", 3, "Good", "")
    val food2 = Food("Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    val food3 = Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")

    assert(!testMenu.addFood(food1, -10))
    testMenu.addFood(food1, 2)
    testMenu.foodList(food1) should be (2.0)
    testMenu.addFood(food2, 2)
    testMenu.foodList(food2) should be (2.0)
    testMenu.addFood(food2, 23)
    testMenu.foodList(food2) should be (25.0)

    testMenu.foodListRaw should have size (1)

    testMenu.foodListCooked should have size (1)

    assert(!testMenu.removeFood(food2, 26))
    testMenu.foodList(food2) should equal (25.0)
    testMenu.removeFood(food2, 23)
    testMenu.foodList(food2) should be (2.0)
    testMenu.removeFood(food2, 2)
    testMenu.foodList(food2) should be (0.0)
    testMenu.removeFood(food3, 5)
  }

  /** Unit test 2 */
  "Fridge get_by functions" should "work correctly" in {
    val testMenu = new FoodMenu()
    testMenu.menu should have size (0)
    val food1 = Food("Food Cookies", Map[Food, Double](), "pcs", "g", 3, "LG1a", "")
    val food2 = Food("Food Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "", "")
    val food3 = Food("", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "AAA", "")
    testMenu.foodList.clear()
    testMenu.addFood(food1, 1)
    testMenu.addFood(food2, 2)
    testMenu.addFood(food3, 3)

    testMenu.getByTags("") should have size (3)
    testMenu.getByTags("A1") should have size (1)
    testMenu.getByTags("A") should have size (2)
    testMenu.getByTags("8") should have size (0)
    testMenu.getByTags("gl") should have size (1)
    testMenu.getByTags(" ") should have size (3)
    
    testMenu.getByName("foo") should have size (2)
    testMenu.getByName("fOod  ") should have size (2)
    testMenu.getByName("") should have size (3)
    testMenu.getByName(" ") should have size (3)
    testMenu.getByName("jfdnsdj") should have size (0)
    testMenu.getByName("s") should have size (2)
    testMenu.getByName("cookies food") should have size (0)

    testMenu.getByAvailability(4) should have size (0)
    testMenu.getByAvailability(3) should have size (1)
    testMenu.getByAvailability(2) should have size (2)
    testMenu.getByAvailability(1) should have size (3)
    testMenu.getByAvailability(0) should have size (3)
  }

  /** Unit test 3 */
  "Menu get/add/del functions" should "work correctly" in {
    val testMenu = new FoodMenu()
    testMenu.menu should have size (0)
    val food1 = Food("Cookies", Map[Food, Double](), "pcs", "g", 3, "Good", "")
    val food2 = Food("Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    val food3 = Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    val food4 = Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    testMenu.addFood(food1, 1)
    testMenu.addFood(food2, 2)
    testMenu.addFood(food3, 3)
    testMenu.addFood(food3, 5)
    food3.setToMenu()
    food1.setToMenu()
    testMenu.menu should have size (2)

    testMenu.addMenu(food2)
    assert(food1.isMenu)
    assert(food2.isMenu)
    assert(food3.isMenu)
    assert(!food4.isMenu)

    testMenu.deleteMenu(food1)
    testMenu.deleteMenu(food4)
    assert(!food1.isMenu)
    assert(food2.isMenu)
    assert(food3.isMenu)
    assert(!food4.isMenu)
  }

  /** Unit test 4 */
  "Menu availability" should "work correctly" in {
    val testMenu = new FoodMenu()
    testMenu.menu should have size (0)
    val food1 = Food("Cookies", Map[Food, Double](), "pcs", "g", 26, "Good", "")
    val food2 = Food("Eggs", Map[Food, Double](), "pcs", "g", 0, "Good", "")
    val food3 = Food("Unknown", Map[Food, Double](food1 -> 2, food2 -> 2), "pcs", "g", 8, "Good", "")
    val food4 = Food("Unknown", Map[Food, Double](food1 -> 3), "pcs", "g", 8, "Good", "")
    val food5 = Food("Unknown", Map[Food, Double](food1 -> 2, food3 -> 2), "pcs", "g", 8, "Good", "")
    
    testMenu.addFood(food1, 26)
    testMenu.addFood(food2, 8)
    testMenu.addFood(food3, 3)
    testMenu.addFood(food4, 5)
    testMenu.addFood(food5, 6)

    testMenu.existingAmount(food1) should be (26.0)
    testMenu.existingAmount(food4) should be (5.0)

    testMenu.checkAvailability(food4) should be (13)
    testMenu.checkAvailability(food3) should be (7)
    testMenu.checkAvailability(food1) should be (26)
    testMenu.checkAvailability(food2) should be (8)
    testMenu.checkAvailability(food5) should be (11)
  }
}