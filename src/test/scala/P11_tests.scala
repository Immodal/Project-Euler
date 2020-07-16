import org.scalatest.FunSuite
import scala.io.Source


class P11Test extends FunSuite {
  
  test("slice") {
    val grid = P11.getGrid(P11.dataPath + "dtest1.txt")
    assert(P11.slice(2, grid, 0, 1, 0, 1) === List(8, 1))
    assert(P11.slice(2, grid, 1, 1, 0, 1) === List(5, 7))
    assert(P11.slice(3, grid, 0, 1, 1, 1) === List(7, 9, 2))
    assert(P11.slice(2, grid, 0, 1, 0, -1) === List())
    assert(P11.slice(2, grid, 0, 1, 1, -1) === List(7, 2))
    assert(P11.slice(2, grid, 1, 1, 1, -1) === List(4, 8))
    assert(P11.slice(2, grid, 0, 1, 2, -1) === List(8, 3))
    assert(P11.slice(3, grid, 0, 1, 0, -1) === List())
    assert(P11.slice(3, grid, 0, 1, 1, -1) === List())
    assert(P11.slice(3, grid, 0, 1, 2, -1) === List(4, 8, 3))
    assert(P11.slice(3, grid, 0, 1, 3, -1) === List(5, 9, 4))
  }

  test("horizontal prod") {
    val grid = P11.getGrid(P11.dataPath + "htest1.txt")
    assert(P11.prod(2, grid, 0, 1) === 30)
    assert(P11.prod(3, grid, 0, 1) === 120)
  }

  test("vertical prod") {
    val grid = P11.getGrid(P11.dataPath + "vtest1.txt")
    assert(P11.prod(2, grid, 1, 0) === 30)
    assert(P11.prod(3, grid, 1, 0) === 120)
  }

  test("diagonal right prod") {
    val grid = P11.getGrid(P11.dataPath + "dtest1.txt")
    assert(P11.prod(2, grid, 1, 1) === 63)
    assert(P11.prod(3, grid, 1, 1) === 315)
  }

    test("diagonal left prod") {
    val grid = P11.getGrid(P11.dataPath + "dtest1.txt")
    assert(P11.prod(2, grid, 1, -1) === 45)
    assert(P11.prod(3, grid, 1, -1) === 180)
  }
}