package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {

  test("average") {
    val temperatures = Iterable(Iterable((Location(1, 1), 4.4), (Location(1, 1), 4.4)))
    val f = Manipulation.average(temperatures)
    assert(f(1, 1) == 4.4)
  }
}