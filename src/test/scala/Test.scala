import cook._
import org.scalatest._
import matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.Map

class UnitTests extends AnyFlatSpec with Matchers {

  /** Unit test 1 */
  "Fridge add/remove functions" should "work correctly" in {
    val testMenu = new Menu()
    val testFridge = testMenu.fridge
    testFridge.foodList.clear()
    val food1 = Food("Cookies", Map[Food, Double](), "pcs", "g", 3, "Good", "")
    val food2 = Food("Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    val food3 = Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")

    assert(!testFridge.addFood(food1, -10))
    testFridge.addFood(food1, 2)
    testFridge.foodList(food1) should be (2.0)
    testFridge.addFood(food2, 2)
    testFridge.foodList(food2) should be (2.0)
    testFridge.addFood(food2, 23)
    testFridge.foodList(food2) should be (25.0)

    testFridge.foodListRaw should have size (1)

    testFridge.foodListCooked should have size (1)

    assert(!testFridge.removeFood(food2, 26))
    testFridge.foodList(food2) should equal (25.0)
    testFridge.removeFood(food2, 23)
    testFridge.foodList(food2) should be (2.0)
    testFridge.removeFood(food2, 2)
    testFridge.foodList(food2) should be (0.0)
    testFridge.removeFood(food3, 5)
  }

  /** Unit test 2 */
  "Fridge get_by functions" should "work correctly" in {
    val testMenu = new Menu()
    val testFridge = testMenu.fridge
    val food1 = Food("Food Cookies", Map[Food, Double](), "pcs", "g", 3, "LG1a", "")
    val food2 = Food("Food Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "", "")
    val food3 = Food("", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "AAA", "")
    testFridge.foodList.clear()
    testFridge.addFood(food1, 1)
    testFridge.addFood(food2, 2)
    testFridge.addFood(food3, 3)

    testFridge.getByTags("") should have size (3)
    testFridge.getByTags("A1") should have size (1)
    testFridge.getByTags("A") should have size (2)
    testFridge.getByTags("8") should have size (0)
    testFridge.getByTags("gl") should have size (1)
    testFridge.getByTags(" ") should have size (3)
    
    testFridge.getByName("foo") should have size (2)
    testFridge.getByName("fOod  ") should have size (2)
    testFridge.getByName("") should have size (3)
    testFridge.getByName(" ") should have size (3)
    testFridge.getByName("jfdnsdj") should have size (0)
    testFridge.getByName("s") should have size (2)
    testFridge.getByName("cookies food") should have size (0)

    testFridge.getByAvailability(4) should have size (0)
    testFridge.getByAvailability(3) should have size (1)
    testFridge.getByAvailability(2) should have size (2)
    testFridge.getByAvailability(1) should have size (3)
    testFridge.getByAvailability(0) should have size (3)
  }

  /** Unit test 3 */
  "Menu get/add/del functions" should "work correctly" in {
    val testMenu = new Menu()
    val testFridge = testMenu.fridge
    testFridge.foodList.clear()
    testMenu.getMenu() should have size (0)
    val food1 = Food("Cookies", Map[Food, Double](), "pcs", "g", 3, "Good", "")
    val food2 = Food("Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    val food3 = Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    val food4 = Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    testFridge.addFood(food1, 1)
    testFridge.addFood(food2, 2)
    testFridge.addFood(food3, 3)
    testFridge.addFood(food3, 5)
    food3.setToMenu()
    food1.setToMenu()
    testMenu.getMenu() should have size (2)

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
    val testMenu = new Menu()
    val testFridge = testMenu.fridge
    testFridge.foodList.clear()
    testMenu.getMenu() should have size (0)
    val food1 = Food("Cookies", Map[Food, Double](), "pcs", "g", 26, "Good", "")
    val food2 = Food("Eggs", Map[Food, Double](), "pcs", "g", 0, "Good", "")
    val food3 = Food("Unknown", Map[Food, Double](food1 -> 2, food2 -> 2), "pcs", "g", 8, "Good", "")
    val food4 = Food("Unknown", Map[Food, Double](food1 -> 3), "pcs", "g", 8, "Good", "")
    val food5 = Food("Unknown", Map[Food, Double](food1 -> 2, food3 -> 2), "pcs", "g", 8, "Good", "")
    
    testFridge.addFood(food1, 26)
    testFridge.addFood(food2, 8)
    testFridge.addFood(food3, 3)
    testFridge.addFood(food4, 5)
    testFridge.addFood(food5, 6)

    testMenu.exisitingAmount(food1) should be (26.0)
    testMenu.exisitingAmount(food4) should be (5.0)

    testMenu.checkAvailability(food4) should be (13)
    testMenu.checkAvailability(food3) should be (7)
    testMenu.checkAvailability(food1) should be (26)
    testMenu.checkAvailability(food2) should be (8)
    testMenu.checkAvailability(food5) should be (11)
  }
}