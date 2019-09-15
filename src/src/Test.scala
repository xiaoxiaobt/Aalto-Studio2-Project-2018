package src

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.Map

class UnitTests {
  var settings = new Settings // Here settings only work for diagnosis.
  def p[T](a: T) = if (settings.diagnosis) print(a.toString)

  def s() = if (settings.diagnosis) print("\t\t\t\tpass\n")

  /** Unit test 1 */
  @Test def test1() {
    p("-Test 1: Fridge add/remove functions\n")
    var test_menu = new Menu()
    var test_fridge = test_menu.fridge
    var food1 = new Food("Cookies", Map[Food, Double](), "pcs", "g", 3, "Good", "")
    var food2 = new Food("Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    var food3 = new Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    p("---add_food")
    assertFalse(test_fridge.add_food(food1, -10))
    test_fridge.add_food(food1, 2)
    assertTrue("addFood", 2.0 == test_fridge.food_list(food1))
    test_fridge.add_food(food2, 2)
    assertTrue("addFood", 2.0 == test_fridge.food_list(food2))
    test_fridge.add_food(food2, 23)
    assertTrue("addFood", 25.0 == test_fridge.food_list(food2))
    s()
    p("---raw_test")
    assertTrue("rawTest", 1 == test_fridge.food_list_raw.size)
    s()
    p("---cooked_test")
    assertTrue("cookedTest", 1 == test_fridge.food_list_cooked.size)
    s()
    p("---remove_food")
    assertFalse(test_fridge.remove_food(food2, 26))
    assertTrue("removeFood", 25.0 == test_fridge.food_list(food2))
    test_fridge.remove_food(food2, 23)
    assertTrue("removeFood", 2.0 == test_fridge.food_list(food2))
    test_fridge.remove_food(food2, 2)
    assertTrue("removeFood", 0.0 == test_fridge.food_list(food2))
    test_fridge.remove_food(food3, 5)
    s()
    p("-Test 1: Done\n")
  }

  /** Unit test 2 */
  @Test def test2() {
    p("-Test 2: Fridge get_by functions\n")
    p("---get_by_tags")
    var test_menu = new Menu()
    var test_fridge = test_menu.fridge
    var food1 = new Food("Food Cookies", Map[Food, Double](), "pcs", "g", 3, "LG1a", "")
    var food2 = new Food("Food Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "", "")
    var food3 = new Food("", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "AAA", "")
    test_fridge.add_food(food1, 1)
    test_fridge.add_food(food2, 2)
    test_fridge.add_food(food3, 3)
    assertEquals("tag", 3, test_fridge.get_by_tags("").size)
    assertEquals("tag", 1, test_fridge.get_by_tags("A1").size)
    assertEquals("tag", 2, test_fridge.get_by_tags("A").size)
    assertEquals("tag", 0, test_fridge.get_by_tags("8").size)
    assertEquals("tag", 1, test_fridge.get_by_tags("gl").size)
    assertEquals("tag", 3, test_fridge.get_by_tags(" ").size)
    s()
    p("---get_by_name")
    assertEquals("name", 2, test_fridge.get_by_name("foo").size)
    assertEquals("name", 2, test_fridge.get_by_name("fOod  ").size)
    assertEquals("name", 3, test_fridge.get_by_name("").size)
    assertEquals("name", 3, test_fridge.get_by_name(" ").size)
    assertEquals("name", 0, test_fridge.get_by_name("jfdnsdj").size)
    assertEquals("name", 2, test_fridge.get_by_name("s").size)
    assertEquals("name", 0, test_fridge.get_by_name("cookies food").size)
    s()
    p("---availability")
    assertEquals("name", 0, test_fridge.get_by_availability(4).size)
    assertEquals("name", 1, test_fridge.get_by_availability(3).size)
    assertEquals("name", 2, test_fridge.get_by_availability(2).size)
    assertEquals("name", 3, test_fridge.get_by_availability(1).size)
    assertEquals("name", 3, test_fridge.get_by_availability(0).size)
    s()
    p("-Test 2: Done\n")
  }

  /** Unit test 3 */
  @Test def test3() {
    p("-Test 3: Menu get/add/del functions\n")
    p("---get_menu")
    var test_menu = new Menu()
    var test_fridge = test_menu.fridge
    assertEquals(0, test_menu.get_menu().size)
    var food1 = new Food("Cookies", Map[Food, Double](), "pcs", "g", 3, "Good", "")
    var food2 = new Food("Eggs", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    var food3 = new Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    var food4 = new Food("Unknown", Map[Food, Double]((food1 -> 2)), "pcs", "g", 8, "Good", "")
    test_fridge.add_food(food1, 1)
    test_fridge.add_food(food2, 2)
    test_fridge.add_food(food3, 3)
    test_fridge.add_food(food3, 5)
    food3.set_to_menu()
    food1.set_to_menu()
    assertEquals(2, test_menu.get_menu().size)
    s()
    p("---add_menu")
    test_menu.add_menu(food2)
    assertTrue(food1.is_menu())
    assertTrue(food2.is_menu())
    assertTrue(food3.is_menu())
    assertFalse(food4.is_menu())
    s()
    p("---del_menu")
    test_menu.del_menu(food1)
    test_menu.del_menu(food4)
    assertFalse(food1.is_menu())
    assertTrue(food2.is_menu())
    assertTrue(food3.is_menu())
    assertFalse(food4.is_menu())
    s()
    p("-Test 3: Done\n")
  }

  /** Unit test 4 */
  @Test def test4() {
    p("-Test 4: Menu availability\n")
    p("---get_menu")
    var test_menu = new Menu()
    var test_fridge = test_menu.fridge
    assertEquals(0, test_menu.get_menu().size)
    var food1 = new Food("Cookies", Map[Food, Double](), "pcs", "g", 26, "Good", "")
    var food2 = new Food("Eggs", Map[Food, Double](), "pcs", "g", 0, "Good", "")
    var food3 = new Food("Unknown", Map[Food, Double](food1 -> 2, food2 -> 2), "pcs", "g", 8, "Good", "")
    var food4 = new Food("Unknown", Map[Food, Double](food1 -> 3), "pcs", "g", 8, "Good", "")
    var food5 = new Food("Unknown", Map[Food, Double](food1 -> 2, food3 -> 2), "pcs", "g", 8, "Good", "")
    test_fridge.add_food(food1, 26)
    test_fridge.add_food(food2, 8)
    test_fridge.add_food(food3, 3)
    test_fridge.add_food(food4, 5)
    test_fridge.add_food(food5, 6)
    s()
    p("---exisiting")
    assertTrue(26.0 == test_menu.exisiting_amount(food1))
    assertTrue(5.0 == test_menu.exisiting_amount(food4))
    s()
    p("---availability")
    assertEquals(13, test_menu.check_availability(food4))
    assertEquals(7, test_menu.check_availability(food3))
    assertEquals(26, test_menu.check_availability(food1))
    assertEquals(8, test_menu.check_availability(food2))
    assertEquals(11, test_menu.check_availability(food5))
    s()
    p("-Test 4: Done\n")
  }
}