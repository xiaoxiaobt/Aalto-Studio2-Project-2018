import cook._
import org.scalatest._
import matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.parallel.mutable.ParHashMap

class UnitTests extends AnyFlatSpec with Matchers {

  "Fridge add/remove functions" should "work correctly" in {
    val testMenu = FoodMenu()
    testMenu.getFoodArray shouldBe empty
    val food1 = Food("Cookies", ParHashMap[Food, Double](), Set[Char](), "Good")
    val food2 =
      Food("Eggs", ParHashMap[Food, Double]((food1 -> 2)), Set[Char](), "Good")
    val food3 = Food(
      "Unknown",
      ParHashMap[Food, Double]((food1 -> 2)),
      Set[Char](),
      "Good"
    )

    testMenu.addFood(food1, -10) shouldBe false
    testMenu.addFood(food1, 2)
    testMenu.foodMap(food1) shouldBe 2
    testMenu.addFood(food2, 2)
    testMenu.foodMap(food2) shouldBe 2
    testMenu.addFood(food2, 23)
    testMenu.foodMap(food2) shouldBe 25

    testMenu.removeFood(food2, 26) shouldBe false
    testMenu.foodMap(food2) shouldBe 25
    testMenu.removeFood(food2, 23)
    testMenu.foodMap(food2) shouldBe 2
    testMenu.removeFood(food2, 2)
    testMenu.foodMap(food2) shouldBe 0
    testMenu.removeFood(food3, 5)
  }

  "Fridge getBy functions" should "work correctly" in {
    val testMenu = FoodMenu()
    testMenu.getFoodArray shouldBe empty
    val food1 =
      Food(
        "Food Cookies",
        ParHashMap[Food, Double](),
        "LG1a".toCharArray.toSet,
        ""
      )
    val food2 =
      Food("Food Eggs", ParHashMap[Food, Double]((food1 -> 2)), Set[Char](), "")
    val food3 =
      Food(
        "",
        ParHashMap[Food, Double]((food1 -> 2)),
        "AAA".toCharArray.toSet,
        ""
      )
    testMenu.foodMap.clear()
    testMenu.addFood(food1, 1)
    testMenu.addFood(food2, 2)
    testMenu.addFood(food3, 3)

    testMenu.getByTags("") should have size (3)
    testMenu.getByTags("A1") should have size (1)
    testMenu.getByTags("A") should have size (2)
    testMenu.getByTags("8") shouldBe empty
    testMenu.getByTags("gl") should have size (1)
    testMenu.getByTags(" ") should have size (3)

    testMenu.getByName("foo") should have size (2)
    testMenu.getByName("fOod  ") should have size (2)
    testMenu.getByName("") should have size (3)
    testMenu.getByName(" ") should have size (3)
    testMenu.getByName("jfdnsdj") shouldBe empty
    testMenu.getByName("s") should have size (2)
    testMenu.getByName("cookies food") shouldBe empty

    testMenu.getByAvailability(4) shouldBe empty
    testMenu.getByAvailability(3) should have size (1)
    testMenu.getByAvailability(2) should have size (2)
    testMenu.getByAvailability(1) should have size (3)
    testMenu.getByAvailability(0) should have size (3)
  }

  "Menu get/add/del functions" should "work correctly" in {
    val testMenu = FoodMenu()
    testMenu.getFoodArray shouldBe empty
    val food1 = Food("Cookies", ParHashMap[Food, Double](), Set[Char](), "Good")
    val food2 =
      Food("Eggs", ParHashMap[Food, Double]((food1 -> 2)), Set[Char](), "Good")
    val food3 = Food(
      "Unknown",
      ParHashMap[Food, Double]((food1 -> 2)),
      Set[Char](),
      "Good"
    )
    val food4 = Food(
      "Unknown",
      ParHashMap[Food, Double]((food1 -> 2)),
      Set[Char](),
      "Good"
    )
    testMenu.addFood(food1, 1)
    testMenu.addFood(food2, 2)
    testMenu.addFood(food3, 3)
    testMenu.addFood(food3, 5)

    testMenu.addMenu(food2)
    food1.isMenu shouldBe false
    food2.isMenu shouldBe true
    food3.isMenu shouldBe false
    food4.isMenu shouldBe false

    testMenu.deleteMenu(food1)
    testMenu.deleteMenu(food4)
    food1.isMenu shouldBe false
    food2.isMenu shouldBe true
    food3.isMenu shouldBe false
    food4.isMenu shouldBe false
  }

  "Menu availability" should "work correctly" in {
    val testMenu = FoodMenu()
    testMenu.getFoodArray shouldBe empty
    val food1 = Food("Cookies", ParHashMap[Food, Double](), Set[Char](), "Good")
    val food2 = Food("Eggs", ParHashMap[Food, Double](), Set[Char](), "Good")
    val food3 = Food(
      "Unknown",
      ParHashMap[Food, Double](food1 -> 2, food2 -> 2),
      Set[Char](),
      "Good"
    )
    val food4 =
      Food("Unknown", ParHashMap[Food, Double](food1 -> 3), Set[Char](), "Good")
    val food5 = Food(
      "Unknown",
      ParHashMap[Food, Double](food1 -> 2, food3 -> 2),
      Set[Char](),
      "Good"
    )

    testMenu.addFood(food1, 26)
    testMenu.addFood(food2, 8)
    testMenu.addFood(food3, 3)
    testMenu.addFood(food4, 5)
    testMenu.addFood(food5, 6)

    testMenu.checkAvailability(food4) shouldBe 13
    testMenu.checkAvailability(food3) shouldBe 7
    testMenu.checkAvailability(food1) shouldBe 26
    testMenu.checkAvailability(food2) shouldBe 8
    testMenu.checkAvailability(food5) shouldBe 11
  }
}
